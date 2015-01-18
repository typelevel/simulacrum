package simulacrum

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Annotation that may be applied to methods on a type that is annotated with `@typeclass`.
 *
 * Doing so changes the code generation strategy used when generating the syntax adapter type.
 * Instead of the type class method name being used, the name specified on this annotation is used.
 * If `alias` is true, two methods are generated, one with the original name and one with the
 * specified name.
 */
class op(name: String, alias: Boolean = false) extends StaticAnnotation

/**
 * Annotation that may be applied to a trait or class of one type parameter to generate
 * boilerplate that makes the type class easier to use.
 *
 * The only type parameter must either by a proper type or a unary type constructor.
 * Types of other shapes, like binary type constructors, are not currently supported.
 *
 * As a result of adding this annotation, the following code is generated in the companion:
 *  - an implicit summoning method, providing syntax like `MyTypeClass[Type]` as a
 *    shortcut for `implicitly[MyTypeClass[Type]]`.
 *  - an implicit class, named `Adapter`, which provides object oriented style
 *    forwarding methods -- aka, syntax.
 */
class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TypeClassMacros.generateTypeClass
}

object TypeClassMacros {
  def generateTypeClass(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def trace(s: => String) = {
      // Macro paradise seems to always output info statements, even without -verbose
      if (sys.props.get("simulacrum.trace").isDefined) c.info(c.enclosingPosition, s, false)
    }

    def determineAdapterMethodName(sourceMethod: DefDef): List[TermName] = {
      val overrides = sourceMethod.mods.annotations.collectFirst {
        case Apply(Select(New(Ident(TypeName(annotationName))), termNames.CONSTRUCTOR), Literal(Constant(alias: String)) :: rest) if annotationName == "op" =>
          val aliasTermName = TermName(reflect.NameTransformer.encode(alias))
          rest match {
            case Nil =>
              List(aliasTermName)
            case Literal(Constant(alias: Boolean)) :: _ =>
              if (alias) List(sourceMethod.name, aliasTermName)
              else List(aliasTermName)
            case AssignOrNamedArg(Ident(TermName("alias")), Literal(Constant(alias: Boolean))) :: _ =>
              if (alias) List(sourceMethod.name, aliasTermName)
              else List(aliasTermName)
            case other =>
              List(aliasTermName)
          }
      }
      overrides getOrElse List(sourceMethod.name)
    }

    def adaptMethodForProperType(tparamName: Name, method: DefDef): List[DefDef] = {
      // Method should only be adapted if the first parameter in the first parameter list
      // matches `tparamName`
      val TargetTypeName = tparamName
      for {
        firstParamList <- method.vparamss.headOption.toList
        firstParam <- firstParamList.headOption.toList
        Ident(TargetTypeName) <- Option(firstParam.tpt).toList
        paramssWithoutFirst = {
          if (firstParamList.tail.isEmpty) method.vparamss.tail
          else firstParamList.tail :: method.vparamss.tail
        }
        paramNamess: List[List[Tree]] = {
          val original = method.vparamss map { _ map { p => Ident(p.name) } }
          original.updated(0, original(0).updated(0, Ident(TermName("self"))))
        }
        rhs = paramNamess.foldLeft(Select(Ident(TermName("typeClass")), method.name): Tree) { (tree, paramNames) =>
          Apply(tree, paramNames)
        }
        name <- determineAdapterMethodName(method)
        val fixedMods = if (method.mods.hasFlag(Flag.OVERRIDE)) Modifiers(Flag.OVERRIDE) else Modifiers(NoFlags)
      } yield DefDef(fixedMods, name, method.tparams, paramssWithoutFirst, method.tpt, rhs)
    }

    def adaptMethodForAppliedType(tparamName: Name, method: DefDef, liftedTypeArg: TypeDef): List[DefDef] = {
      // Method should only be adapted if the first parameter in the first parameter list
      // is an F[X] for some (potentially applied) type X
      val TargetTypeName = tparamName
      (for {
        firstParamList <- method.vparamss.headOption.toList
        firstParam <- firstParamList.headOption.toList
        AppliedTypeTree(Ident(TargetTypeName), arg :: Nil) <- Option(firstParam.tpt).toList
      } yield {
        arg match {
          // If arg == Ident(TypeName("...")), method can be lifted directly
          case Ident(simpleArg) =>
            // Rewrites all occurrences of simpleArg to liftedTypeArg.name
            val SimpleArg = simpleArg
            def rewrite(t: Tree): Tree = t match {
              case Ident(SimpleArg) => Ident(liftedTypeArg.name)
              case AppliedTypeTree(x, ys) => AppliedTypeTree(rewrite(x), ys map { y => rewrite(y) })
              // TODO This is woefully incomplete - no attempt is made at rewriting the types of trees that appear in rhs
              case other => other
            }

            val paramssFixed = {
              val withoutFirst = {
                if (firstParamList.tail.isEmpty) method.vparamss.tail
                else firstParamList.tail :: method.vparamss.tail
              }
              withoutFirst map { _ map { param =>
                ValDef(param.mods, param.name, rewrite(param.tpt), rewrite(param.rhs))
              }}
            }

            val paramNamess: List[List[Tree]] = {
              val original = method.vparamss map { _ map { p => Ident(p.name) } }
              original.updated(0, original(0).updated(0, Ident(TermName("self"))))
            }

            val rhs = paramNamess.foldLeft(Select(Ident(TermName("typeClass")), method.name): Tree) { (tree, paramNames) =>
              Apply(tree, paramNames)
            }

            val fixedTParams = method.tparams.filter { _.name != simpleArg }
            val fixedMods = if (method.mods.hasFlag(Flag.OVERRIDE)) Modifiers(Flag.OVERRIDE) else Modifiers(NoFlags)

            determineAdapterMethodName(method) map { name =>
              DefDef(fixedMods, name, fixedTParams, paramssFixed, rewrite(method.tpt), rhs)
            }

          case AppliedTypeTree(g, a) =>
            // We need an additional implicit evidence param
            // E.g., op[G[_], A](F[G[A]], ...) => F[$A].op[G[_], A](...)(implicit ev $A =:= G[A])
            trace(s"Not adapting ${method.name} - adaptation of methods on shape F[G[X]] not supported")
            List(method)
        }
      }).flatten
    }

    def adaptMethods(typeClass: ClassDef, tparamName: Name, proper: Boolean, liftedTypeArg: Option[TypeDef]): List[DefDef] = {
      val typeClassMethods = typeClass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }
      typeClassMethods.flatMap { method =>
        val adapted = if (proper) adaptMethodForProperType(tparamName, method) else adaptMethodForAppliedType(tparamName, method, liftedTypeArg.get)
        adapted
      }
    }

    def generateAdapter(typeClass: ClassDef, tparam: TypeDef, proper: Boolean, liftedTypeArg: Option[TypeDef]) = {
      val adaptedMethods = adaptMethods(typeClass, tparam.name, proper, liftedTypeArg)
      val adapterBases: List[Tree] = {
        typeClass.impl.parents.flatMap {
          case AppliedTypeTree(Ident(TypeName(parentTypeClassName)), arg :: Nil) =>
            val typeArgs = arg :: (if (proper) Nil else List(Ident(liftedTypeArg.get.name)))
            Some(AppliedTypeTree(Select(Ident(TermName(parentTypeClassName)), TypeName("Adapter")), typeArgs))
          case other => None
        }
      }
      if (proper) {
        q"""trait Adapter[${tparam}] extends ..${adapterBases} {
          def typeClass: ${typeClass.name}[${tparam.name}]
          def self: ${tparam.name}
          ..$adaptedMethods

        }"""
      } else {
        q"""
        trait Adapter[${tparam}, ${liftedTypeArg.get}] extends ..${adapterBases} {
          def typeClass: ${typeClass.name}[${tparam.name}]
          def self: ${tparam.name}[${liftedTypeArg.get.name}]
          ..$adaptedMethods
        }"""
      }
    }

    def generateCompanion(typeClass: ClassDef, tparam: TypeDef, proper: Boolean, comp: Tree) = {

      val summoner = q"def apply[$tparam](implicit instance: ${typeClass.name}[${tparam.name}]): ${typeClass.name}[${tparam.name}] = instance"

      val liftedTypeArg = if (proper) None else Some {
        // We have a TypeClass[F[_ >: L <: U]], so let's create a F[X >: L <: U] for a fresh name X
        // For example:
        // TypeDef(
        //   Modifiers(PARAM), TypeName("F"), List(
        //     TypeDef(Modifiers(PARAM), typeNames.WILDCARD, List(), TypeBoundsTree(Ident(TypeName("Lower")), Ident(TypeName("Upper"))))
        //   ), TypeBoundsTree(EmptyTree, EmptyTree))
        val TypeDef(_, _, tparamtparams, _) = tparam
        tparamtparams.find { _.name == typeNames.WILDCARD } match {
          case None => c.abort(c.enclosingPosition, "Cannot find a wildcard type is supposed unary type constructor")
          case Some(TypeDef(mods, _, tpps, rhs)) =>
            // TODO: Might be better to create a new mods off the existing one, minus the PARAM flag
            val fixedMods = Modifiers(NoFlags, mods.privateWithin, mods.annotations)
            // TODO: we should rewrite typeNames.WILDCARD in tpps and rhs
            val liftedTypeArgName = TypeName(c.freshName())
            TypeDef(fixedMods, liftedTypeArgName, tpps, rhs)
        }
      }

      val adapter = generateAdapter(typeClass, tparam, proper, liftedTypeArg)
      trace(s"Generated adapter class for ${typeClass.name}:\n" + adapter)

      val adapterConversion = {
        if (proper) {
          // Generate an implicit conversion from A to Adapter[A]
          q"implicit def Adapter[$tparam](target: ${tparam.name})(implicit tc: ${typeClass.name}[${tparam.name}]): Adapter[${tparam.name}] = new Adapter[${tparam.name}] { val self = target; val typeClass = tc }"
        } else {
          // Generate an implicit conversion from F[A] to Adapter[F, A]
          val typeArg = liftedTypeArg.get
          q"implicit def Adapter[$tparam, $typeArg](target: ${tparam.name}[${typeArg.name}])(implicit tc: ${typeClass.name}[${tparam.name}]): Adapter[${tparam.name}, ${typeArg.name}] = new Adapter[${tparam.name}, ${typeArg.name}] { val self = target; val typeClass = tc }"
        }
      }

      val lowerTypeClassName = TermName(typeClass.name.toString.updated(0, typeClass.name.toString.charAt(0).toLower))

      val q"object $name extends ..$bases { ..$body }" = comp
      q"""
        object $name extends ..$bases {
          ..$body
          $summoner
          $adapter
          $adapterConversion
        }
      """
    }

    def modify(typeClass: ClassDef, companion: Option[ModuleDef]) = {
      val (tparam, proper) = typeClass.tparams match {
        case hd :: Nil =>
          hd.tparams.size match {
            case 0 => (hd, true)
            case 1 => (hd, false)
            case n => c.abort(c.enclosingPosition, "@typeclass may only be applied to types that take a single proper type or type constructor")
          }
        case other => c.abort(c.enclosingPosition, "@typeclass may only be applied to types that take a single type parameter")
      }

      val modifiedTypeClass = typeClass

      val modifiedCompanion = generateCompanion(typeClass, tparam, proper, companion match {
        case Some(c) => c
        case None => q"object ${typeClass.name.toTermName} {}"
      })

      c.Expr(q"""
        $modifiedTypeClass
        $modifiedCompanion
      """)
    }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => modify(typeClass, None)
      case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => modify(typeClass, Some(companion))
      case other :: Nil =>
        c.abort(c.enclosingPosition, "@typeclass can only be applied to traits or abstract classes that take 1 type parameter which is either a proper type or a type constructor")
    }
  }
}
