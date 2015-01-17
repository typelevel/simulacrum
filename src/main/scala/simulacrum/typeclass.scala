package simulacrum

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.Context

class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro TypeClassMacros.generateTypeClass
}

object TypeClassMacros {
  def generateTypeClass(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def trace(s: => String) = () //println(s)

    def adaptMethodForProperType(tparamName: Name, method: DefDef): Option[DefDef] = {
      // Method should only be adapted if the first parameter in the first parameter list
      // matches `tparamName`
      val TargetTypeName = tparamName
      for {
        firstParamList <- method.vparamss.headOption
        firstParam <- firstParamList.headOption
        Ident(TargetTypeName) <- Option(firstParam.tpt)
      } yield {
        val paramssWithoutFirst = {
          if (firstParamList.tail.isEmpty) method.vparamss.tail
          else firstParamList.tail :: method.vparamss.tail
        }

        val paramNamess: List[List[Tree]] = {
          val original = method.vparamss map { _ map { p => Ident(p.name) } }
          original.updated(0, original(0).updated(0, Ident(TermName("self"))))
        }

        val rhs = paramNamess.foldLeft(Select(Ident(TermName("typeClass")), method.name): Tree) { (tree, paramNames) =>
          Apply(tree, paramNames)
        }

        val adapted = DefDef(Modifiers(Flag.FINAL), method.name, method.tparams, paramssWithoutFirst, method.tpt, rhs)
        adapted
      }
    }

    def adaptMethodForAppliedType(tparamName: Name, method: DefDef, liftedTypeArgName: TypeName): Option[DefDef] = {
      // Method should only be adapted if the first parameter in the first parameter list
      // is an F[X] for some (potentially applied) type X
      val TargetTypeName = tparamName
      for {
        firstParamList <- method.vparamss.headOption
        firstParam <- firstParamList.headOption
        AppliedTypeTree(Ident(TargetTypeName), arg :: Nil) <- Option(firstParam.tpt)
      } yield {
        arg match {
          // If arg == Ident(TypeName("...")), method can be lifted directly
          case Ident(simpleArg) =>
            // Rewrites all occurrences of simpleArg to liftedTypeArgName
            val SimpleArg = simpleArg
            def rewrite(t: Tree): Tree = t match {
              case Ident(SimpleArg) => Ident(liftedTypeArgName)
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

            val adapted = DefDef(Modifiers(Flag.FINAL), method.name, fixedTParams, paramssFixed, rewrite(method.tpt), rhs)
            adapted

          case AppliedTypeTree(g, a) =>
            // We need an additional implicit evidence param
            // E.g., op[G[_], A](F[G[A]], ...) => F[$A].op[G[_], A](...)(implicit ev $A =:= G[A])
            trace(s"Not adapting ${method.name} - adaptation of methods on shape F[G[X]] not supported")
            method
        }
      }
    }

    def adaptMethods(typeClass: ClassDef, tparamName: Name, proper: Boolean, liftedTypeArgName: TypeName): List[DefDef] = {
      val typeClassMethods = typeClass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }
      typeClassMethods.flatMap { method =>
        trace(s"Adapting method as syntax for a $tparamName: ${method.mods} ${method.name}")
        val adapted = if (proper) adaptMethodForProperType(tparamName, method) else adaptMethodForAppliedType(tparamName, method, liftedTypeArgName)
        trace(s"Adapted to: $adapted")
        adapted
      }
    }

    def generateAdapter(typeClass: ClassDef, tparam: TypeDef, proper: Boolean) = {
      val liftedTypeArgName = TypeName(c.freshName())
      val adaptedMethods = adaptMethods(typeClass, tparam.name, proper, liftedTypeArgName)
      val adapterBases: List[Tree] = {
        typeClass.impl.parents.flatMap {
          case AppliedTypeTree(Ident(TypeName(parentTypeClassName)), arg :: Nil) =>
            Some(AppliedTypeTree(Select(Ident(TermName(parentTypeClassName)), TypeName("Adapter")), arg :: Nil))
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
        trait Adapter[${tparam}, $liftedTypeArgName] {
          def typeClass: ${typeClass.name}[${tparam.name}]
          def self: ${tparam.name}[$liftedTypeArgName]
          ..$adaptedMethods
        }"""
      }
    }

    def generateCompanion(typeClass: ClassDef, tparam: TypeDef, proper: Boolean, comp: Tree) = {

      val summoner = q"def apply[$tparam](implicit instance: ${typeClass.name}[${tparam.name}]): ${typeClass.name}[${tparam.name}] = instance"

      val adapter = generateAdapter(typeClass, tparam, proper)

      val adapterConversion = {
        if (proper) {
          // Generate an implicit conversion from A to Adapter[A]
          q"implicit def Adapter[$tparam](target: ${tparam.name})(implicit tc: ${typeClass.name}[${tparam.name}]): Adapter[${tparam.name}] = new Adapter[${tparam.name}] { val self = target; val typeClass = tc }"
        } else {
          // Generate an implicit conversion from F[A] to Adapter[F, A]
          val typeArg = TypeName(c.freshName())
          q"implicit def Adapter[$tparam, $typeArg](target: ${tparam.name}[$typeArg])(implicit tc: ${typeClass.name}[${tparam.name}]): Adapter[${tparam.name}, $typeArg] = new Adapter[${tparam.name}, $typeArg] { val self = target; val typeClass = tc }"
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
            case n => c.abort(typeClass.pos, "@typeclass may only be applied to types that take a single proper type or type constructor")
          }
        case other => c.abort(typeClass.pos, "@typeclass may only be applied to types that take a single type parameter")
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
      case other :: Nil => c.abort(other.pos, "@typeclass can only be applied to traits or abstract classes that take 1 type parameter which is either a proper type or a type constructor")
    }
  }
}
