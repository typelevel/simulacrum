package simulacrum

import org.scalatest.{ WordSpec, Matchers }

class Examples extends WordSpec with Matchers {

  "the @typeclass annotation" should {

    "support type classes that are polymorphic over a proper type" in {

      @typeclass trait Semigroup[A] {
        @op("|+|") def append(x: A, y: A): A
      }
      @typeclass trait Monoid[A] extends Semigroup[A] {
        def id: A
      }

      object IntMonoids {
        implicit val Additive: Monoid[Int] = new Monoid[Int] {
          def id = 0
          def append(x: Int, y: Int) = x + y
        }

        implicit val Multiplicative: Monoid[Int] = new Monoid[Int] {
          def id = 1
          def append(x: Int, y: Int) = x * y
        }
      }

      {
        import IntMonoids.Additive
        Monoid[Int] shouldBe IntMonoids.Additive
        Semigroup[Int] shouldBe IntMonoids.Additive

        import Monoid.Adapter
        1 |+| 2 shouldBe 3
      }

      {
        import IntMonoids.Multiplicative
        Monoid[Int] shouldBe IntMonoids.Multiplicative
        Semigroup[Int] shouldBe IntMonoids.Multiplicative

        import Monoid.Adapter
        1 |+| 2 shouldBe 2
      }
    }

    "support type classes that are polymorphic over a unary type constructor" in {

      @typeclass trait Functor[F[_]] {
        def map[A, B](fa: F[A])(f: A => B): F[B]
      }
      @typeclass trait Applicative[F[_]] extends Functor[F] {
        def pure[A](a: => A): F[A]
        def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
        override def map[A, B](fa: F[A])(f: A => B): F[B] =
          ap(fa)(pure(f))
      }
      @typeclass trait Monad[F[_]] extends Applicative[F] {
        @op(">>=", alias = true) def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
        override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] =
          flatMap(f)(map(fa))
        override def map[A, B](fa: F[A])(f: A => B): F[B] =
          flatMap(fa)(a => pure(f(a)))
      }
      @typeclass trait PlusEmpty[F[_]] {
        def empty[A]: F[A]
      }
      @typeclass trait MonadPlus[F[_]] extends Monad[F] with PlusEmpty[F] {
        def filter[A](fa: F[A])(f: A => Boolean) =
          flatMap(fa)(a => if (f(a)) pure(a) else empty[A])
      }

      sealed trait Maybe[+A]
      object Maybe {
        case class Just[A](value: A) extends Maybe[A]
        case object Empty extends Maybe[Nothing]

        def just[A](a: A): Maybe[A] = Just(a)
        def empty[A]: Maybe[A] = Empty

        implicit val instance: MonadPlus[Maybe] = new MonadPlus[Maybe] {
          def pure[A](a: => A) = just(a)
          def empty[A] = Maybe.empty[A]
          def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa match {
            case Just(a) => f(a)
            case e @ Empty => e
          }
        }
      }

      import MonadPlus.Adapter

      // We get the map function from Functor.Adapter, which is the super-super-super class of MonadPlus.Adapter
      Maybe.just(1) map ((_: Int) + 1) shouldBe Maybe.just(2)

      // We get >>= syntax as an alias for flatMap from the super-class of MonadPlus.Adapter
      val recriprocal: Int => Maybe[Double] = x => if (x == 0) Maybe.empty else Maybe.just(1.0 / x)
      Maybe.just(1) >>= recriprocal

      // We get map from Functor.Adapter, flatMap from Monad.Adapter, and filter from MonadPlus.Adapter
      def div(x: Maybe[Int], y: Maybe[Int]): Maybe[Double] = for {
        xx <- x
        yy <- y
        if (yy > 0)
      } yield xx.toDouble / yy

      div(Maybe.just(1), Maybe.just(2)) shouldBe Maybe.just(1.toDouble / 2)
      div(Maybe.just(1), Maybe.empty) shouldBe Maybe.empty
    }
  }
}
