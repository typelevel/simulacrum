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
        import Semigroup.Adapter
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
        Monoid.Adapter(1).append(2) shouldBe 3
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
        import Functor.Adapter
        List(1, 2, 3).as(0) shouldBe List(0, 0, 0)
      }

      "supports type class inheritance" in {
        @typeclass trait Monad[G[_]] extends Functor[G] {
          def pure[A](a: => A): G[A]
          def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
          def map[A, B](ga: G[A])(f: A => B) = flatMap(ga) { a => pure(f(a)) }
        }
        implicit val monadList: Monad[List] = new Monad[List] {
          def pure[A](a: => A) = List(a)
          def flatMap[A, B](ga: List[A])(f: A => List[B]): List[B] = ga.flatMap(f)
        }
        Monad[List].flatMap(List(1, 2))(x => List(x, x)) shouldBe List(1, 1, 2, 2)
        Monad.Adapter(List(1, 2)).flatMap { x => List(x, x) } shouldBe List(1, 1, 2, 2)
      }
    }
  }
}
