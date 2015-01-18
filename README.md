simulacrum
==========

[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/mpilquist/simulacrum?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

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
  implicit def apply[A](implicit tc: Semigroup[A]): Semigroup[A] = tc

  implicit class Adapter[A](val self: A)(implicit val typeClass: Semigroup[A]) {
    def append(y: A): A = typeClass.append(self, y)
    def |+|(y: A): A = typeClass.append(self, y)
  }
}
```

This isn't exactly what's generated -- for instance, the `Adapter` class is really generated as a trait to support various subtyping cases. Subtyping of type classes is supported (e.g., you can define a `Monoid` type class that extends `Semigroup` and the generated code adapts accordingly). Higher kinds are also supported -- specifically, type classes that are polymorphic over type constructors, like `Functor`. The current implementation only supports unary type constructors, but support for binary type constructors is planned.

This allows usage like:

```scala
implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
  def append(x: Int, y: Int) = x + y
}

import Semigroup.Adapter
1 |+| 2 // 3
```

This project currently only supports Scala 2.11. The project is based on macro paradise. To use the project, add the following to your build.sbt:

```scala
resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.1.0-SNAPSHOT"
```

Macro paradise must exist in projects which use `@typeclass`, but code that dependencies on the generated type classes do not need macro paradise.

Feedback is much appreciated. The generated code is a result of working with project leads of a variety of open source projects that use type classes. However, there's certainly room for improvement, so please open issues or PRs containg feedback. Also, see the [TODO.md](TODO.md) file for near term improvements.

