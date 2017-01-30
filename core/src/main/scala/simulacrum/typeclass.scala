package simulacrum

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import macrocompat._

/**
 * Annotation that may be applied to methods on a type that is annotated with `@typeclass`.
 *
 * Doing so changes the code generation strategy used when generating the syntax ops type.
 * Instead of the type class method name being used, the name specified on this annotation is used.
 * If `alias` is true, two methods are generated, one with the original name and one with the
 * specified name.
 */
@compileTimeOnly("op annotation should have been removed by simulacrum but was not")
class op(name: String, alias: Boolean = false) extends StaticAnnotation

/**
 * Annotation that may be applied to methods on a type that is annotated with `@typeclass`.
 *
 * Doing so results in the method being excluded from the generated syntax ops type.
 */
@compileTimeOnly("noop annotation should have been removed by simulacrum but was not")
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
@compileTimeOnly("typeclass annotation should have been removed by simulacrum but was not")
class typeclass(excludeParents: List[String] = Nil, generateAllOps: Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TypeClassMacros.generateTypeClass
}

@bundle
class TypeClassMacros(val c: Context) {
  import c.universe._

  def generateTypeClass(annottees: c.Expr[Any]*): c.Expr[Any] = {

    /** Can't have variant modifiers on method type parameters.
     */
    def eliminateVariance(tparam: TypeDef): TypeDef = {
      // If there's another way to do this I'm afraid I don't know it.
      val u        = c.universe.asInstanceOf[c.universe.type with scala.reflect.internal.SymbolTable]
      val tparam0  = tparam.asInstanceOf[u.TypeDef]
      val badFlags = (Flag.COVARIANT | Flag.CONTRAVARIANT).asInstanceOf[Long]
      val fixedMods = tparam0.mods & ~badFlags
      TypeDef(fixedMods.asInstanceOf[c.universe.Modifiers], tparam.name, tparam.tparams, tparam.rhs)
    }

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

    class FoldTransformer(transformers: List[Transformer]) extends Transformer {
      override def transform(t: Tree): Tree = super.transform(transformers.foldLeft(t)((prev, transformer) => transformer.transform(prev)))
    }

    case class Arguments(parentsToExclude: Set[TypeName], generateAllOps: Boolean)

    val typeClassArguments: Arguments = c.prefix.tree match {
      case Apply(_, args) =>
        val excludeParents: Set[TypeName] = args.collectFirst { case q"excludeParents = $exclusions" =>
          c.eval(c.Expr[List[String]](exclusions)).map { n => TypeName(n) }.toSet
        }.getOrElse(Set.empty)
        val generateAllOps: Boolean = args.collectFirst { case q"generateAllOps = $gen" =>
          c.eval(c.Expr[Boolean](gen))
        }.getOrElse(true)
        Arguments(excludeParents, generateAllOps)
      case other => c.abort(c.enclosingPosition, "not possible - macro invoked on type that does not have @typeclass: " + showRaw(other))
    }

    def determineOpsMethodName(sourceMethod: DefDef): List[TermName] = {
      val suppress = sourceMethod.mods.annotations.filter { ann =>
        val typed = c.typecheck(ann)
        typed.tpe.typeSymbol.fullName match {
          case "simulacrum.noop" => true
          case _ => false
        }
      }.nonEmpty
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
        val overrides = sourceMethod.mods.annotations.flatMap { ann =>
          val typed = c.typecheck(ann)
          typed.tpe.typeSymbol.fullName match {
            case "simulacrum.op" =>
              val q"new ${_}(${Literal(Constant(alias: String))}, ..$rest)" = typed
              List(genAlias(alias, rest))
            case _ => Nil
          }
        }
        if (overrides.isEmpty) List(sourceMethod.name.toTermName) else overrides.flatten
      }
    }

    def filterSimulacrumAnnotations(mods: Modifiers): Modifiers = {
      val filteredAnnotations = mods.annotations.filter { ann =>
        val typed = c.typecheck(ann)
        typed.tpe.typeSymbol.fullName match {
          case "simulacrum.op" => false
          case "simulacrum.noop" => false
          case _ => true
        }
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

    /** Adapts methods of the n order kind `F[A0, AN]` and method `def method[A0, AN, B](arg0: F[A0, AN], arg1, ...)` to def method[B](arg1, ...)*/
    def adaptMethodForAppliedType(tcInstanceName: TermName, tparamName: Name, method: DefDef, liftedTypeArgs: List[TypeDef]): List[DefDef] = {
      val TargetTypeName = tparamName
      (for {
        firstParamList <- method.vparamss.headOption.toList
        firstParam <- firstParamList.headOption.toList
        AppliedTypeTree(Ident(TargetTypeName), args) <- Option(firstParam.tpt).toList
        if !method.mods.hasFlag(Flag.OVERRIDE)
      } yield {
        val typeArgs = method.tparams.map { _.name }.toSet

        //Check if the first argument uses any of the typeargs of the method
        val simpleArgs = {
          def extract(tree: Tree): Option[Name] = tree match {
            case Ident(name: TypeName) if typeArgs contains name => Some(name)
            //for arguments of the form F[G[A]] where A is a typearg of the method
            case tq"$ctor[..$targs]" => targs.foldLeft(Option.empty[Name]) { (_, targ) => extract(targ) }
            case other => Option.empty
          }
          args.zipWithIndex.map {
            case (arg, idx) =>
              val simpleArgOpt = extract(arg)
              (arg, simpleArgOpt, liftedTypeArgs(idx), simpleArgOpt.map(arg equalsStructure Ident(_)).getOrElse(false))
            }
        }

        val skipMethod = !simpleArgs.foldLeft(true)(_ && _._2.isDefined)

        if(skipMethod) List.empty else {
          //rewrites all occurences of any of the args which are defined on the method to the lifted arg
          val rewriteSimpleArgs = new FoldTransformer(simpleArgs.foldLeft(List.empty[Transformer]) {
            case (ts, (_, Some(simpleArg), liftedTypeArg, _)) => new RewriteTypeName(from = simpleArg.toTypeName, to = liftedTypeArg.name) :: ts
          })
          //evidence for type args which are nested
          val equalityEvidences = simpleArgs.filterNot(_._4).map {
            case (arg, _, liftedTypeArg, _) =>
              val tEq = tq"_root_.scala.Predef.<:<[${liftedTypeArg.name}, $arg]"
              ValDef(Modifiers(Flag.IMPLICIT), TermName(c.freshName("ev")), tEq, EmptyTree)
          }
          //params to strip from method signature because they are defined on
          val removeTParams = simpleArgs.filter(_._4).map(_._2.get).toSet
          val withoutFirst = if (firstParamList.tail.isEmpty) method.vparamss.tail else firstParamList.tail :: method.vparamss.tail
          val withRewrittenFirst = withoutFirst map { _ map { param =>
            ValDef(param.mods, param.name, rewriteSimpleArgs.transform(param.tpt), rewriteSimpleArgs.transform(param.rhs))
          }}

          val paramssFixed = if(equalityEvidences.isEmpty) withRewrittenFirst else {
            if(withRewrittenFirst.nonEmpty && withRewrittenFirst.last.head.mods.hasFlag(Flag.IMPLICIT))
              withRewrittenFirst.init ++ List(equalityEvidences ++ withRewrittenFirst.last)
            else withRewrittenFirst ++ List(equalityEvidences)
          }

            val paramNamess: List[List[Tree]] = {
              val original = method.vparamss map { _ map { p => Ident(p.name) } }
              val replacement = if (equalityEvidences.isEmpty) q"self" else q"self.asInstanceOf[${tparamName.toTypeName}[..$args]]"
              original.updated(0, original(0).updated(0, replacement))
            }

            val mtparamss = if(equalityEvidences.isEmpty) method.tparams.map(t => tq"""${t.name}""").map(rewriteSimpleArgs.transform) else Nil

            val rhs = paramNamess.foldLeft(q"""$tcInstanceName.${method.name.toTermName}[..$mtparamss]""": Tree) { (tree, paramNames) =>
              Apply(tree, paramNames)
            }

            val fixedTParams = method.tparams.filter { tparam => !removeTParams.contains(tparam.name) }

            determineOpsMethodName(method) map { name =>
              // Important: let the return type be inferred here, so the return type doesn't need to be rewritten
              q"def $name[..$fixedTParams](...$paramssFixed) = $rhs"
            }
          }
      }).flatten
    }

    def adaptMethods(typeClass: ClassDef, tcInstanceName: TermName, tparamName: Name, proper: Boolean, liftedTypeArgs: List[TypeDef]): List[DefDef] = {
      val typeClassMethods = typeClass.impl.children.collect {
        case m: DefDef if !m.mods.hasFlag(Flag.PRIVATE) && !m.mods.hasFlag(Flag.PROTECTED) => m
      }
      typeClassMethods.flatMap { method =>
        val adapted =
          if (proper) adaptMethodForProperType(tcInstanceName, tparamName, method)
          else adaptMethodForAppliedType(tcInstanceName, tparamName, method, liftedTypeArgs)
        adapted
      }
    }

    def targetTypeTree(tparam: TypeDef, proper: Boolean, liftedTypeArgs: List[TypeDef]): Tree = if(proper) tq"${tparam.name}" else tq"""${tparam.name}[..${liftedTypeArgs.map(_.name)}]"""

    def refinedInstanceTypeTree(typeClass: ClassDef, tparam: TypeDef, instance: TermName): Tree = {
      val abstractTypeMembers = typeClass.impl.children.collect { case t @ TypeDef(mods, _, _, _) if mods.hasFlag(Flag.DEFERRED) => t }
      if (abstractTypeMembers.isEmpty) {
        tq"${typeClass.name}[${tparam.name}]"
      } else {
        val refinements = abstractTypeMembers.map { case TypeDef(mods, name, tparams, rhs) =>
          TypeDef(NoMods, name, tparams, tq"$instance.$name")
        }
        tq"${typeClass.name}[${tparam.name}]{ ..$refinements }"
      }
    }

    def generateOps(typeClass: ClassDef, tcInstanceName: TermName, tparam: TypeDef, proper: Boolean, liftedTypeArgs: List[TypeDef]): (ClassDef, Set[TypeName]) = {
      val adaptedMethods = adaptMethods(typeClass, tcInstanceName, tparam.name, proper, liftedTypeArgs)
      val tparams = List(eliminateVariance(tparam)) ++ liftedTypeArgs
      val tparamNames = tparams.map { _.name }
      val targetType = targetTypeTree(tparam, proper, liftedTypeArgs)
      val shouldImportTcMembers = {
        val typeMembersOfTypeClass = typeClass.impl.children.collect { case t: TypeDef => t }
        typeMembersOfTypeClass.exists { td =>
          adaptedMethods.exists { method =>
            method.exists {
              case Ident(tpname) => tpname == td.name
              case _ => false
            }
          }
        }
      }
      val importTcMembers = if (shouldImportTcMembers) List(q"""import $tcInstanceName._""") else Nil

      val opsTrait = q"""trait Ops[..$tparams] {
        type TypeClassType <: ${typeClass.name}[${tparam.name}]
        val $tcInstanceName: TypeClassType
        ..$importTcMembers
        def self: $targetType
        ..$adaptedMethods
      }"""

      val reservedTypeNames = adaptedMethods.flatMap(_.tparams.map(_.name)).toSet ++ tparamNames
      (opsTrait, reservedTypeNames)
    }

    def generateAllOps(typeClass: ClassDef, tcInstanceName: TermName, tparam: TypeDef, liftedTypeArgs: List[TypeDef]): ClassDef = {
      val tparams = List(tparam) ++ liftedTypeArgs
      val tparamNames = tparams.map { _.name }
      val tcargs = typeClass.mods.annotations.flatMap { ann =>
        val typed = c.typecheck(ann)
        if (typed.tpe.typeSymbol.fullName == "simulacrum.typeclass") {
          val q"new ${_}(..${args})" = typed
          List(args)
        } else Nil
      }
      val typeClassParents: List[TypeName] = typeClass.impl.parents.collect {
        case tq"${Ident(parentTypeClassTypeName)}[${_}]" => parentTypeClassTypeName.toTypeName
      }
      val allOpsParents = typeClassParents collect {
        case parent if !(typeClassArguments.parentsToExclude contains parent) =>
          tq"${parent.toTermName}.AllOps[..$tparamNames]"
      }
      val unknownParentExclusions = (typeClassArguments.parentsToExclude -- typeClassParents.toSet).toList.map(_.toString).sorted
      if (unknownParentExclusions.nonEmpty) {
        c.error(c.enclosingPosition, s"@typeclass excludes unknown parent types: ${unknownParentExclusions.mkString}")
      }
      q"""trait AllOps[..$tparams] extends Ops[..$tparamNames] with ..$allOpsParents {
        val $tcInstanceName: TypeClassType
      }"""
    }

    def generateCompanion(typeClass: ClassDef, tparam0: TypeDef, proper: Boolean, comp: Tree) = {
      val tparam = eliminateVariance(tparam0)
      val instance = TermName("instance")
      val refinedType = refinedInstanceTypeTree(typeClass, tparam, instance)
      val summoner = q"@scala.inline def apply[$tparam](implicit $instance: ${typeClass.name}[${tparam.name}]): $refinedType = instance"

      val liftedTypeArgs = if (proper) List.empty[TypeDef] else {
        // We have a TypeClass[F[_ >: L <: U]], so let's create a F[X >: L <: U] for a fresh name X
        // For example:
        // TypeDef(
        //   Modifiers(PARAM), TypeName("F"), List(
        //     TypeDef(Modifiers(PARAM), typeNames.WILDCARD, List(), TypeBoundsTree(Ident(TypeName("Lower")), Ident(TypeName("Upper"))))
        //   ), TypeBoundsTree(EmptyTree, EmptyTree))
        val TypeDef(_, _, tparamtparams, _) = tparam
        val ftss = tparamtparams.filter(_.name == typeNames.WILDCARD)
        if(ftss.isEmpty)
          c.abort(c.enclosingPosition, "Cannot find a wildcard type in supposed n-arity type constructor")
        else {
          val liftedTypeArgName = TypeName(c.freshName(s"lta"))
          ftss.foldLeft(0 -> List.empty[TypeDef]) {
            case ((i, ts), q"$mods type ${_}[..$tpps] = $rhs") =>
              val fixedMods = Modifiers(NoFlags, mods.privateWithin, mods.annotations)
              val tname = TypeName(c.freshName(s"lta$i"))
              object rewriteWildcard extends Transformer {
                override def transform(t: Tree): Tree = t match {
                  case Ident(typeNames.WILDCARD) => super.transform(Ident(tname))
                  case _ => super.transform(t)
                }
              }
              (i + 1) -> (rewriteWildcard.transformTypeDefs(List(TypeDef(fixedMods, tname, tpps, rhs))).head :: ts)
          }._2.reverse
        }
     }

      val tcInstanceName = TermName("typeClassInstance")

      val (opsTrait, opsReservedTParamNames) = generateOps(typeClass, tcInstanceName, tparam, proper, liftedTypeArgs)
      val allOpsTrait = generateAllOps(typeClass, tcInstanceName, tparam, liftedTypeArgs)

      def generateOpsImplicitConversion(opsType: TypeName, methodName: TermName) = {
        val tparams = List(eliminateVariance(tparam)) ++ liftedTypeArgs
        val tparamNames = tparams.map(_.name)
        val targetType = targetTypeTree(tparam, proper, liftedTypeArgs)
        val instance = TermName("tc")
        val refinedType = refinedInstanceTypeTree(typeClass, tparam, instance)
        // Suppressing `ImplicitConversion` is probably necessary, but it should
        // be possible to avoid `ExplicitImplicitTypes` (see
        // puffnfresh/wartremover#226).
        q"""
        @java.lang.SuppressWarnings(scala.Array(
          "org.wartremover.warts.ExplicitImplicitTypes",
          "org.wartremover.warts.ImplicitConversion"))
        implicit def $methodName[..$tparams](target: $targetType)(implicit $instance: ${typeClass.name}[${tparam.name}]): $opsType[..$tparamNames]{ type TypeClassType = $refinedType} =
          new $opsType[..$tparamNames] { type TypeClassType = $refinedType; val self = target; val $tcInstanceName = $instance.asInstanceOf[$refinedType] }
        """
      }

      val toOpsTrait = {
        val toOpsTraitName = TypeName(s"To${typeClass.name}Ops")
        val method = generateOpsImplicitConversion(opsTrait.name, TermName(s"to${typeClass.name}Ops"))
        q"trait $toOpsTraitName { $method }"
      }

      val nonInheritedOpsConversion = {
        val method = generateOpsImplicitConversion(opsTrait.name, TermName(s"to${typeClass.name}Ops"))
        q"object nonInheritedOps extends ${toOpsTrait.name}"
      }

      val allOpsConversion = {
        val method = generateOpsImplicitConversion(TypeName("AllOps"), TermName(s"toAll${typeClass.name}Ops"))
        q"object ops { $method }"
      }

      val opsMembers: List[Tree] = {
        val ops = List(opsTrait, toOpsTrait, nonInheritedOpsConversion)
        val allOps = if (typeClassArguments.generateAllOps) List(allOpsTrait, allOpsConversion) else Nil
        ops ++ allOps
      }

      val q"$mods object $name extends ..$bases { ..$body }" = comp
      val companion = q"""
        $mods object $name extends ..$bases {
          ..$body
          $summoner
          ..$opsMembers
        }
      """

      // Rewrite liftedTypeArg.name to something easier to read
      val potentialNames = ('A' to 'Z').map(ch => TypeName(ch.toString)).filter(nme => !opsReservedTParamNames.contains(nme))

      liftedTypeArgs.foldLeft((companion: Tree) -> potentialNames) {
        case ((prev, namesLeft), lta) =>
          val newName = namesLeft.head
          new RewriteTypeName(from = lta.name, to = newName).transform(prev) -> namesLeft.tail
      }._1
    }

    def modify(typeClass: ClassDef, companion: Option[ModuleDef]) = {
      val (tparam, proper) = typeClass.tparams match {
        case hd :: Nil => (hd, hd.tparams.isEmpty)
        case _ => c.abort(c.enclosingPosition, "@typeclass may only be applied to types that take a single type parameter")
      }

      val modifiedTypeClass = {
        val filteredBody = typeClass.impl.body.map {
          case q"$mods def $name[..$tparamss](...$vparamss): $tpt = $rhs" =>
            q"${filterSimulacrumAnnotations(mods)} def $name[..$tparamss](...$vparamss): $tpt = $rhs"
          case other => other
        }
        val modifiedParents = {
          val makeUniversal = {
            typeClass.impl.parents match {
              case tq"_root_.scala.Any" :: Nil => false
              case tq"scala.Any" :: Nil => false
              case tq"_root_.scala.AnyRef" :: Nil => true
              case tq"scala.AnyRef" :: Nil => true
              case _ => false
            }
          }
          val universal = if (makeUniversal) List(tq"_root_.scala.Any") else typeClass.impl.parents
          universal :+ tq"_root_.scala.Serializable"
        }
        val filteredImpl = Template(modifiedParents, typeClass.impl.self, filteredBody)
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
