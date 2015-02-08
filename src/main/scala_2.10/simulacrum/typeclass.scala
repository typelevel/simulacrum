package simulacrum

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

/**
 * Annotation that may be applied to methods on a type that is annotated with `@typeclass`.
 *
 * Doing so changes the code generation strategy used when generating the syntax ops type.
 * Instead of the type class method name being used, the name specified on this annotation is used.
 * If `alias` is true, two methods are generated, one with the original name and one with the
 * specified name.
 */
class op(name: String, alias: Boolean = false) extends StaticAnnotation

/**
 * Annotation that may be applied to methods on a type that is annotated with `@typeclass`.
 *
 * Doing so results in the method being excluded from the generated syntax ops type.
 */
class noop() extends StaticAnnotation

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
 *  - a trait, named `Ops`, which provides object oriented style forwarding
 *    methods -- aka, syntax -- for the methods defined directly on the type class.
 *  - a trait, named `AllOps`, which extends `Ops` and the `Ops` traits for any
 *    super types.
 *  - a trait, named `ToMyTypeClassOps`, which provides an implicit conversion
 *    that enables use of the `Ops` trait.
 *  - an object, named `ops`, which provides an implicit conversion to the
 *    `AllOps` trait.
 *
 * As a result, the ops can be used by either importing `MyTypeClass.ops._` or
 * by mixing `MyTypeClass.ToMyTypeClassOps` in to a type.
 */
class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeClassMacros.generateTypeClass
}

object TypeClassMacros {
  def generateTypeClass(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def freshName(prefix: String) = c.fresh(prefix)
    def TermName(name: String) = newTermName(name)
    def TypeName(name: String) = newTypeName(name)
    val typeNames = tpnme
    def showCode(t: Tree): String = show(t)

    def trace(s: => String) = {
      // Macro paradise seems to always output info statements, even without -verbose
      if (sys.props.get("simulacrum.trace").isDefined) c.info(c.enclosingPosition, s, false)
    }

    class RewriteTypeName(from: TypeName, to: TypeName) extends Transformer {
      override def transform(t: Tree): Tree = t match {
        case Ident(name) if name == from => super.transform(Ident(to))
        case TypeDef(mods, name, tparams, rhs) if name == from => super.transform(TypeDef(mods, to, tparams, rhs))
        case other => super.transform(other)
      }
    }

    def determineOpsMethodName(sourceMethod: DefDef): List[TermName] = {
      val suppress = sourceMethod.mods.annotations.collectFirst {
        case q"new noop()" => ()
        case q"new simulacrum.noop()" => ()
      }.isDefined
      if (suppress) Nil
      else {
        def genAlias(alias: String, rest: List[Tree]) = {
          val aliasTermName = TermName(reflect.NameTransformer.encode(alias))
          rest match {
            case Nil =>
              List(aliasTermName)
            case Literal(Constant(alias: Boolean)) :: _ =>
              if (alias) List(sourceMethod.name.toTermName, aliasTermName)
              else List(aliasTermName)
            case q"alias = ${Literal(Constant(alias: Boolean))}" :: _ =>
              if (alias) List(sourceMethod.name.toTermName, aliasTermName)
              else List(aliasTermName)
            case other =>
              List(aliasTermName)
          }
        }
        val overrides = sourceMethod.mods.annotations.collect {
          case q"new op(${Literal(Constant(alias: String))}, ..$rest)" => genAlias(alias, rest)
          case q"new simulacrum.op(${Literal(Constant(alias: String))}, ..$rest)" => genAlias(alias, rest)
        }
        if (overrides.isEmpty) List(sourceMethod.name.toTermName) else overrides.flatten
      }
    }

    def filterSimulacrumAnnotations(mods: Modifiers): Modifiers = {
      val filteredAnnotations = mods.annotations.filter {
        case q"new typeclass(..${_})" => false
        case q"new op(..${_})" => false
        case q"new noop(..${_})" => false
        case q"new simulacrum.${_}(..${_})" => false
        case other => true
      }
      Modifiers(mods.flags, mods.privateWithin, filteredAnnotations)
    }

    def adaptMethodForProperType(tcInstanceName: TermName, tparamName: Name, method: DefDef): List[DefDef] = {
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
          original.updated(0, original(0).updated(0, q"self"))
        }
        rhs = paramNamess.foldLeft(Select(Ident(tcInstanceName), method.name): Tree) { (tree, paramNames) =>
          Apply(tree, paramNames)
        }
        name <- determineOpsMethodName(method)
        if !method.mods.hasFlag(Flag.OVERRIDE)
      } yield DefDef(Modifiers(NoFlags), name, method.tparams, paramssWithoutFirst, method.tpt, rhs)
    }

    def adaptMethodForAppliedType(tcInstanceName: TermName, tparamName: Name, method: DefDef, liftedTypeArg: TypeDef): List[DefDef] = {
      // Method should only be adapted if the first parameter in the first parameter list
      // is an F[X] for some (potentially applied) type X
      val TargetTypeName = tparamName
      (for {
        firstParamList <- method.vparamss.headOption.toList
        firstParam <- firstParamList.headOption.toList
        AppliedTypeTree(Ident(TargetTypeName), arg :: Nil) <- Option(firstParam.tpt).toList
        if !method.mods.hasFlag(Flag.OVERRIDE)
      } yield {

        val typeArgs = method.tparams.map { _.name }.toSet

        val simpleArgOpt: Option[Name] = {
          def extract(tree: Tree): Option[Name] = tree match {
            case Ident(name: TypeName) if typeArgs contains name => Some(name)
            case tq"$ctor[..$targs]" =>
              targs.foldLeft(None: Option[Name]) { (acc, targ) => extract(targ) }
            case other => None
          }
          extract(arg)
        }

        simpleArgOpt match {
          case None => Nil
          case Some(simpleArg) =>

            // Rewrites all occurrences of simpleArg to liftedTypeArg.name
            val rewriteSimpleArg = new RewriteTypeName(from = simpleArg.toTypeName, to = liftedTypeArg.name)

            val (paramssFixed, removeSimpleArgTParam) = {
              val withoutFirst = {
                if (firstParamList.tail.isEmpty) method.vparamss.tail
                else firstParamList.tail :: method.vparamss.tail
              }
              val withRewrittenFirst = withoutFirst map { _ map { param =>
                ValDef(param.mods, param.name, rewriteSimpleArg.transform(param.tpt), rewriteSimpleArg.transform(param.rhs))
              }}
              if (arg equalsStructure Ident(simpleArg)) {
                (withRewrittenFirst, true)
              } else {
                val typeEqualityType = tq"${liftedTypeArg.name} =:= $arg"
                val equalityEvidence = ValDef(Modifiers(Flag.IMPLICIT), TermName(freshName("ev")), typeEqualityType, EmptyTree)
                val updatedParamss = {
                  if (withRewrittenFirst.nonEmpty && withRewrittenFirst.last.head.mods.hasFlag(Flag.IMPLICIT))
                    withRewrittenFirst.init ++ List(equalityEvidence +: withRewrittenFirst.last)
                  else {
                    withRewrittenFirst ++ List(List(equalityEvidence))
                  }
                }
                (updatedParamss, false)
              }
            }

            val paramNamess: List[List[Tree]] = {
              val original = method.vparamss map { _ map { p => Ident(p.name) } }
              val replacement = if (removeSimpleArgTParam) q"self" else q"self.asInstanceOf[${tparamName.toTypeName}[$arg]]"
              original.updated(0, original(0).updated(0, replacement))
            }

            val rhs = paramNamess.foldLeft(Select(Ident(tcInstanceName), method.name): Tree) { (tree, paramNames) =>
              Apply(tree, paramNames)
            }

            val fixedTParams = if (removeSimpleArgTParam) method.tparams.filter { _.name != simpleArg } else method.tparams

            determineOpsMethodName(method) map { name =>
              // Important: let the return type be inferred here, so the return type doesn't need to be rewritten
              q"def $name[..$fixedTParams](...$paramssFixed) = $rhs"
            }
        }
      }).flatten
    }

    def adaptMethods(typeClass: ClassDef, tcInstanceName: TermName, tparamName: Name, proper: Boolean, liftedTypeArg: Option[TypeDef]): List[DefDef] = {
      val typeClassMethods = typeClass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }
      typeClassMethods.flatMap { method =>
        val adapted =
          if (proper) adaptMethodForProperType(tcInstanceName, tparamName, method)
          else adaptMethodForAppliedType(tcInstanceName, tparamName, method, liftedTypeArg.get)
        adapted
      }
    }

    def generateOps(typeClass: ClassDef, tcInstanceName: TermName, tparam: TypeDef, proper: Boolean, liftedTypeArg: Option[TypeDef]): (ClassDef, Set[TypeName]) = {
      val adaptedMethods = adaptMethods(typeClass, tcInstanceName, tparam.name, proper, liftedTypeArg)
      val tparams = List(tparam) ++ liftedTypeArg
      val tparamNames = tparams.map { _.name }
      val targetType = liftedTypeArg.map(lta => tq"${tparam.name}[${lta.name}]").getOrElse(tq"${tparam.name}")

      val opsTrait = q"""trait Ops[..$tparams] {
        def $tcInstanceName: ${typeClass.name}[${tparam.name}]
        def self: $targetType
        ..$adaptedMethods
      }"""

      val reservedTypeNames = adaptedMethods.flatMap(_.tparams.map(_.name)).toSet ++ tparamNames
      (opsTrait, reservedTypeNames)
    }

    def generateAllOps(typeClass: ClassDef, tcInstanceName: TermName, tparam: TypeDef, liftedTypeArg: Option[TypeDef]): ClassDef = {
      val tparams = List(tparam) ++ liftedTypeArg
      val tparamNames = tparams.map { _.name }
      val parents = typeClass.impl.parents.collect {
        case tq"${Ident(parentTypeClassTypeName)}[$arg]" =>
          tq"${parentTypeClassTypeName.toTermName}.AllOps[..$tparamNames]"
      }
      q"""trait AllOps[..$tparams] extends Ops[..$tparamNames] with ..$parents {
        def $tcInstanceName: ${typeClass.name}[${tparam.name}]
      }"""
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
          case None => c.abort(c.enclosingPosition, "Cannot find a wildcard type in supposed unary type constructor")
          case Some(q"$mods type ${_}[..$tpps] = $rhs") =>
            // TODO: Might be better to create a new mods off the existing one, minus the PARAM flag
            val fixedMods = Modifiers(NoFlags, mods.privateWithin, mods.annotations)
            val liftedTypeArgName = TypeName(freshName("lta"))
            object rewriteWildcard extends Transformer {
              override def transform(t: Tree): Tree = t match {
                case Ident(typeNames.WILDCARD) => super.transform(Ident(liftedTypeArgName))
                case other => super.transform(t)
              }
            }
            rewriteWildcard.transformTypeDefs(List(TypeDef(fixedMods, liftedTypeArgName, tpps, rhs))).head
        }
      }

      val tcInstanceName = TermName("typeClassInstance")

      val (opsTrait, opsReservedTParamNames) = generateOps(typeClass, tcInstanceName, tparam, proper, liftedTypeArg)
      val allOpsTrait = generateAllOps(typeClass, tcInstanceName, tparam, liftedTypeArg)

      def generateOpsImplicitConversion(opsType: TypeName, methodName: TermName) = {
        val tparams = List(tparam) ++ liftedTypeArg
        val tparamNames = tparams.map(_.name)
        val targetType = liftedTypeArg.map(lta => tq"${tparam.name}[${lta.name}]").getOrElse(tq"${tparam.name}")
        q"""
        implicit def $methodName[..$tparams](target: $targetType)(implicit tc: ${typeClass.name}[${tparam.name}]): $opsType[..$tparamNames] =
          new $opsType[..$tparamNames] { val self = target; val $tcInstanceName = tc }
        """
      }

      val toOpsTrait = {
        val toOpsTraitName = TypeName(s"To${typeClass.name}Ops")
        val method = generateOpsImplicitConversion(opsTrait.name, TermName(s"to${typeClass.name}Ops"))
        q"trait $toOpsTraitName { $method }"
      }

      val allOpsConversion = {
        val method = generateOpsImplicitConversion(TypeName("AllOps"), TermName(s"toAll${typeClass.name}Ops"))
        q"object ops { $method }"
      }

      val opsMembers: List[Tree] = {
        List(opsTrait, toOpsTrait, allOpsTrait, allOpsConversion)
      }

      val q"object $name extends ..$bases { ..$body }" = comp
      val companion = q"""
        object $name extends ..$bases {
          ..$body
          $summoner
          ..$opsMembers
        }
      """

      // Rewrite liftedTypeArg.name to something easier to read
      liftedTypeArg.fold(companion: Tree) { lta =>
        val potentialNames = ('A' to 'Z').map(ch => TypeName(ch.toString)).toList
        val newLiftedTypeArgName = potentialNames.find(nme => !(opsReservedTParamNames contains nme))
        newLiftedTypeArgName.fold(companion: Tree) { newName =>
          new RewriteTypeName(from = lta.name, to = newName).transform(companion)
        }
      }
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

      val modifiedTypeClass = {
        val filteredBody = typeClass.impl.body.map {
          case q"$mods def $name[..$tparams](...$vparamss): $tpt = $rhs" =>
            q"${filterSimulacrumAnnotations(mods)} def $name[..$tparams](...$vparamss): $tpt = $rhs"
          case other => other
        }
        val filteredImpl = Template(typeClass.impl.parents, typeClass.impl.self, filteredBody)
        ClassDef(filterSimulacrumAnnotations(typeClass.mods), typeClass.name, typeClass.tparams, filteredImpl)
      }

      val modifiedCompanion = generateCompanion(typeClass, tparam, proper, companion match {
        case Some(c) => c
        case None => q"object ${typeClass.name.toTermName} {}"
      })

      val result = c.Expr(q"""
        $modifiedTypeClass
        $modifiedCompanion
      """)

      trace(s"Generated type class ${typeClass.name}:\n" + showCode(result.tree))

      result
    }

    annottees.map(_.tree) match {
      case (typeClass: ClassDef) :: Nil => modify(typeClass, None)
      case (typeClass: ClassDef) :: (companion: ModuleDef) :: Nil => modify(typeClass, Some(companion))
      case other :: Nil =>
        c.abort(c.enclosingPosition, "@typeclass can only be applied to traits or abstract classes that take 1 type parameter which is either a proper type or a type constructor")
    }
  }
}
