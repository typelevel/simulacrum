simulacrum
==========

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mpilquist/simulacrum?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Note on maintenance** 

This project is only maintained for Scala 2.x. No new features are developed, but bug fix releases will still be made available. For Dotty/Scala 3, please use [simulacrum-scalafix](https://github.com/typelevel/simulacrum-scalafix), which is a set of Scalafix rewrites that mirror simulacrum's features.

Type classes rock. Alas, their encoding in Scala requires a lot of boilerplate, which doesn't rock. There is inconsistency between projects, where type classes are encoded differently. There is inconsistency within projects, where object-oriented forwarders (aka. ops, syntax) accidentally differ in exact parameter lists or forwarders are missing where they are expected to be. Even in disciplined teams, the bike-shedding opportunities alone are a source of lost productivity.

This project addresses these concerns by introducing first class support for type classes in Scala 2.11. For example:

```scala
import simulacrum._

@typeclass trait Semigroup[A] {
  @op("|+|") def append(x: A, y: A): A
}
```

Given this definition, something similar to the following is generated at compile time:

```scala
trait Semigroup[A] {
  def append(x: A, y: A): A
}

object Semigroup {
  def apply[A](implicit instance: Semigroup[A]): Semigroup[A] = instance

  trait Ops[A] {
    def typeClassInstance: Semigroup[A]
    def self: A
    def |+|(y: A): A = typeClassInstance.append(self, y)
  }

  trait ToSemigroupOps {
    implicit def toSemigroupOps[A](target: A)(implicit tc: Semigroup[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object nonInheritedOps extends ToSemigroupOps

  trait AllOps[A] extends Ops[A] {
    def typeClassInstance: Semigroup[A]
  }

  object ops {
    implicit def toAllSemigroupOps[A](target: A)(implicit tc: Semigroup[A]): AllOps[A] = new AllOps[A] {
      val self = target
      val typeClassInstance = tc
    }
  }
}
```

The `Ops` trait contains extension methods for a value of type `A` for which there's a `Semigroup[A]` instance available. The `ToSemigroupOps` trait contains an implicit conversion from an `A` to an `Ops[A]`. The `ToSemigroupOps` trait can be mixed in to a class in order to get access to the extension methods. It can also be mixed in to an object, along with other `ToXyzOps` traits, in order to provide a single mass import object.

The `AllOps` trait mixes in `Ops` along with the `AllOps` traits of all super types. In this example, there are no super types, but we'll look at such an example soon. Finally, the `ops` object provides an implicit conversion that can be directly imported in order to use the extension methods.

```scala
implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
  def append(x: Int, y: Int) = x + y
}

import Semigroup.ops._
1 |+| 2 // 3
```

Subtyping of type classes is supported. For example:

```scala
@typeclass trait Monoid[A] extends Semigroup[A] {
  def id: A
}
```

Generates:

```scala
trait Monoid[A] extends Semigroup[A] {
  def id: A
}

object Monoid {
  def apply[A](implicit instance: Monoid[A]): Monoid[A] = instance

  trait Ops[A] {
    def typeClassInstance: Monoid[A]
    def self: A
  }

  trait ToMonoidOps {
    implicit def toMonoidOps[A](target: A)(implicit tc: Monoid[A]): Ops[A] = new Ops[A] {
      val self = target
      val typeClassInstance = tc
    }
  }

  trait AllOps[A] extends Ops[A] with Semigroup.AllOps[A] {
    def typeClassInstance: Monoid[A]
  }

  object ops {
    implicit def toAllMonoidOps[A](target: A)(implicit tc: Monoid[A]): AllOps[A] = new AllOps[A] {
      val self = target
      val typeClassInstance = tc
    }
  }
}
```

In this example, the `id` method was not lifted to the `Ops` trait because it is not an extension method for an `A` value. Even though there were no such methods, an empty `Ops` trait was still generated. This is important for various subtyping scenarios as they relate to separate compilation.

Higher kinds are also supported -- specifically, type classes that are polymorphic over type constructors, like `Functor`. The current implementation only supports unary type constructors, but support for binary type constructors is planned.

This allows usage like:

See [the examples](examples/src/test/scala/simulacrum/examples/examples.scala) for more.

## Usage

The generated code supports two modes of method extension. Consider the case of the `Monad` typeclass: it is a subtype of `Applicative` which is, itself, a subtype of `Functor`. After extending our monad with the `Monad` trait, we need to bring our implicits into scope.

```scala
/**
 * We can simply import the contents of Monad's ops
 *  object to get it and all ancestor methods:
 */
import Monad.ops._

/**
 * Alternatively, we can use the ToMonadOps trait
 *  to mixin just the operations we want:
 */
object NoMapForMonad extends ToMonadOps with ToApplicativeOps {}
import NoMapForMonad._
```

Note that the second approach will not include the `map` operation of its grandparent type, `Functor`. The benefit of this second approach is that a collection of method extensions can be brought into scope all at once. Indeed, the typeclasses of operations imported in this second fashion need not be related.

## Including Simulacrum

This project supports Scala 2.11, 2.12, and 2.13. The project is based on macro paradise. To use the project, add the following to your build.sbt:

```scala
libraryDependencies += "org.typelevel" %% "simulacrum" % "1.0.1"

// For Scala 2.11-2.12
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

// For Scala 2.13+
scalacOptions += "-Ymacro-annotations"
```

Macro paradise must exist in projects which use `@typeclass`, but code that depends on the generated type classes do not need macro paradise.

Feedback is much appreciated. The generated code is a result of working with project leads of a variety of open source projects that use type classes. However, there's certainly room for improvement, so please open issues or PRs containing feedback.

## Known Limitations

 - Only type classes that abstract over a proper type or a unary type constructor are currently supported. This will be extended to binary type constructors in the future, and perhaps n-ary type constructors.
 - When defining a type class as a subtype of another type class, and defining an abstract member of the super type concretely in the sub type, the `override` keyword must be used. For example, defining `map` in terms of `flatMap` requires `override def map[A, B](...)`.
 - See the GitHub issues list for other known limitations and please open issues for any other limitations you encounter. If you suspect a problem, it may be helpful to run with the `simulacrum.trace` system property (e.g., `sbt -Dsimulacrum.trace compile`), which adds a significant amount of logging to the compiler output.

Code of Conduct
---------------

See the [Code of Conduct](CODE_OF_CONDUCT.md).
