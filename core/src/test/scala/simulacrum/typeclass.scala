package simulacrum

import org.scalatest.{ WordSpec, Matchers }

class TypeClassTest extends WordSpec with Matchers {

  "the @typeclass annotation" should {

    "support type classes that are polymorphic over a proper type," which {
      @typeclass trait Semigroup[T] {
        def append(x: T, y: T): T
        def appendCurried(x: T)(y: T): T = append(x, y)
      }

      implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
        def append(x: Int, y: Int) = x + y
      }

      "generates an implicit summoning method in companion" in {
        Semigroup[Int] shouldBe semigroupInt
      }

      "generates object oriented style forwarding methods" in {
        "1 append 2 shouldBe 3" shouldNot compile
        import Semigroup.ops._
        1 append 2 shouldBe 3
        1 appendCurried 2 shouldBe 3
      }

      "supports type class inheritance" in {
        @typeclass trait Monoid[X] extends Semigroup[X] {
          def id: X
        }
        implicit val monoidInt: Monoid[Int] = new Monoid[Int] {
          def append(x: Int, y: Int) = x + y
          def id = 0
        }
        Monoid[Int].id shouldBe 0
        Monoid[Int].append(1, 2) shouldBe 3
        import Monoid.ops._
        1 append 2 shouldBe 3
      }

      "supports pre-existing companions" in {
        @typeclass trait Sg[A] {
          def op(x: A, y: A): A
        }
        object Sg {
          def foo = 1
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def op(x: Int, y: Int) = x + y
        }

        Sg[Int].op(1, 2) shouldBe 3
        Sg.foo shouldBe 1
      }

      "supports changing the name of adapted methods" in {
        @typeclass trait Sg[A] {
          @op("|+|") def append(x: A, y: A): A
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def append(x: Int, y: Int) = x + y
        }

        import Sg.ops._
        1 |+| 2 shouldBe 3
        "1 append 2" shouldNot compile
      }

      "supports aliasing the name of adapted methods" in {
        @typeclass trait Sg[A] {
          @op("|+|", alias = true) def append(x: A, y: A): A
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def append(x: Int, y: Int) = x + y
        }

        import Sg.ops._
        1 |+| 2 shouldBe 3
        1 append 2 shouldBe 3
      }

      "supports aliasing the name of adapted methods (without named arg)" in {
        @typeclass trait Sg[A] {
          @op("|+|", true) def append(x: A, y: A): A
          @simulacrum.op("~", true) def foo(x: A, y: A): A = append(x, y)
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def append(x: Int, y: Int) = x + y
        }

        import Sg.ops._
        1 |+| 2 shouldBe 3
        1 append 2 shouldBe 3
        1 foo 2 shouldBe 3
        1 ~ 2 shouldBe 3
      }

      "supports suppression of adapter methods" in {
        @typeclass trait Sg[A] {
          @noop def append(x: A, y: A): A
          @simulacrum.noop def foo(x: A, y: A): A = append(x, y)
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def append(x: Int, y: Int) = x + y
        }

        import Sg.ops._
        "1 append 2 shouldBe 3" shouldNot compile
        "1 foo 2 shouldBe 3" shouldNot compile
      }

      "supports type bounds on type class type param" in {
        trait Upper
        trait Lower extends Upper
        trait Mixin[Y]
        @typeclass trait Sub[X <: Upper] { def id(x: X): X = x }
        @typeclass trait Sup[X >: Lower] { def id(x: X): X = x }
        @typeclass trait Both[X >: Lower <: Upper] { def id(x: X): X = x }
        @typeclass trait Lots[X >: Lower with Mixin[Int] <: Upper] { def id(x: X): X = x }
      }

      "supports diamond inheritance" in {
        @typeclass trait Foo[A] { def op(x: A): A }
        @typeclass trait Bar[A] extends Foo[A] {
          def bar(x: A): A
          override def op(x: A): A = bar(x)
        }
        @typeclass trait Baz[A] extends Foo[A] {
          def baz(x: A): A
          override def op(x: A): A = baz(x)
        }
        @typeclass trait Qux[A] extends Bar[A] with Baz[A] { def qux(x: A): A }
        implicit val qint = new Qux[Int] {
          def bar(x: Int) = x
          def baz(x: Int) = -x
          def qux(x: Int) = x * 2
        }
        import Qux.ops._
        1.op shouldBe -1 // Linearization causes the op override from bar to take precedence
      }

      "supports type classes that extends traits that are not type classes" in {
        trait Show[A] { def show(a: A): String }
        @typeclass(excludeParents = List("Show")) trait ShowingSemigroup[A] extends Show[A] { def append(x: A, y: A): A }
      }

      "report compilation failures when excludeParents attribute references unknown parent" in {
        trait Show[A] { def show(a: A): String }
        """
        @typeclass(excludeParents = List("Show", "Foo")) trait ShowingSemigroup[A] extends Show[A] { def append(x: A, y: A): A }
        """ shouldNot compile
      }

      "supports suppressing generation of the AllOps trait and the ops object" in {
        @typeclass(generateAllOps = false) trait Show[A] { def show(a: A): String }
        "trait foo extends Show.AllOps" shouldNot compile
        "Show.ops" shouldNot compile
      }

      "supports universal traits" in {
        @typeclass trait Univseral[A] extends Any {
          def foo: A
        }
      }
    }

    "support type classes that are polymorphic over a type constructor," which {
      @typeclass trait Functor[F[_]] {
        def map[A, B](fa: F[A])(f: A => B): F[B]
        def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)
        def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
        def foo[G[_], A](fga: F[G[A]]): G[F[A]] = ???
      }

      implicit val functorList: Functor[List] = new Functor[List] {
        def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
      }

      "generates an implicit summoning method in companion" in {
        Functor[List] shouldBe functorList
      }

      "generates object oriented style forwarding methods" in {
        "List(1, 2, 3).as(0) shouldBe List(0, 0, 0)" shouldNot compile
        import Functor.ops._
        List(1, 2, 3).as(0) shouldBe List(0, 0, 0)
      }

      "supports type class inheritance" in {
        @typeclass trait Monad[G[_]] extends Functor[G] {
          def pure[A](a: => A): G[A]
          def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
          override def map[A, B](ga: G[A])(f: A => B) = flatMap(ga) { a => pure(f(a)) }
        }
        implicit val monadList: Monad[List] = new Monad[List] {
          def pure[A](a: => A) = List(a)
          def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
        }
        Monad[List].flatMap(List(1, 2))(x => List(x, x)) shouldBe List(1, 1, 2, 2)
        Monad.ops.toAllMonadOps(List(1, 2)).flatMap { x => List(x, x) } shouldBe List(1, 1, 2, 2)
      }

      "supports changing the name of adapted methods" in {
        @typeclass trait Monad[G[_]] extends Functor[G] {
          def pure[A](a: => A): G[A]
          @op(">>=") def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
          override def map[A, B](ga: G[A])(f: A => B) = flatMap(ga) { a => pure(f(a)) }
        }
        implicit val monadList: Monad[List] = new Monad[List] {
          def pure[A](a: => A) = List(a)
          def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
        }
        import Monad.ops._
        val twice: Int => List[Int] = x => List(x, x)
        (List(1, 2, 3) >>= twice) shouldBe List(1, 1, 2, 2, 3, 3)
        "Monad.Ops(List(1, 2, 3)) flatMap twice" shouldNot compile
      }

      "supports aliasing the name of adapted methods" in {
        @typeclass trait Monad[G[_]] extends Functor[G] {
          def pure[A](a: => A): G[A]
          @op(">>=", alias = true) def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
          override def map[A, B](ga: G[A])(f: A => B) = flatMap(ga) { a => pure(f(a)) }
        }
        implicit val monadList: Monad[List] = new Monad[List] {
          def pure[A](a: => A) = List(a)
          def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
        }
        import Monad.ops._
        val twice: Int => List[Int] = x => List(x, x)
        (List(1, 2, 3) >>= twice) shouldBe List(1, 1, 2, 2, 3, 3)
        Monad.ops.toAllMonadOps(List(1, 2, 3)) flatMap twice shouldBe List(1, 1, 2, 2, 3, 3)
      }

      "supports type bounds on type class type param" in {
        trait Upper
        trait Lower extends Upper
        trait Mixin[Y]
        @typeclass trait Sub[F[_] <: Upper] { def id[A](x: F[A]): F[A] = x }
        @typeclass trait Sup[G[_] >: Lower] { def id[B](x: G[B]): G[B] = x }
        @typeclass trait Both[H[_] >: Lower <: Upper] { def id[B](x: H[B]): H[B] = x }
        @typeclass trait Lots[I[_] >: Lower with Mixin[Int] <: Upper] { def id[B](x: I[B]): I[B] = x }
        @typeclass trait TypeConstructorBounded[F[_ >: Lower <: Upper]] { def id[A >: Lower <: Upper](x: F[A]): F[A] = x }
      }

      "lifted type argument in method bodies are supported" in {
        object typeClasses {
          @typeclass trait Monoid[A] { def append(x: A, y: A): A; def id: A }
          @typeclass trait Foldable[F[_]] {
            def foldLeft[A, B](fa: F[A])(b: B)(f: (B, A) => B): B
            def concatenate[A: Monoid](fa: F[A]): A = foldLeft(fa)(Monoid[A].id)((acc, a) => Monoid[A].append(acc, a))
          }
        }
        import typeClasses._
        implicit val intMonoid: Monoid[Int] = new Monoid[Int] { def append(x: Int, y: Int) = x + y; def id = 0 }
        implicit val listFoldable: Foldable[List] = new Foldable[List] {
          def foldLeft[A, B](fa: List[A])(b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
        }
        import Foldable.ops._
        List(1, 2, 3).concatenate shouldBe 6
      }

      "lifted type argument in return type is rewritten correctly" in {
        @typeclass trait Foo[F[_]] { def toEither[A](fa: F[A]): Either[String, A] }
        implicit val listFoo = new Foo[List] { def toEither[A](fa: List[A]) = if (fa.isEmpty) Left("empty") else Right(fa.head) }
        import Foo.ops._
        Nil.toEither shouldBe Left("empty")
      }

      "supports syntax for an F[G[A]]" in {
        @typeclass trait Bar[F[_]]
        @typeclass trait Foo[F[_]] {
          def foo[G[_], A, B](fa: F[A])(f: A => G[B]): G[F[B]]
          def bar[G[_]: Bar, A](fa: F[G[A]]): G[F[A]] = foo(fa)(identity)
        }
        implicit val barOption: Bar[Option] = new Bar[Option] {}
        implicit val fooOption: Foo[Option] = new Foo[Option] {
          def foo[G[_], A, B](fa: Option[A])(f: A => G[B]): G[Option[B]] = null.asInstanceOf[G[Option[B]]]
        }
        import Foo.ops._
        Option(Option(1)).bar
      }
    }
  }
}
