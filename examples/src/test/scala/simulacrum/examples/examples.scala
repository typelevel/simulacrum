package simulacrum.examples

import simulacrum._

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Examples extends AnyWordSpec with Matchers {

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

        import Monoid.ops._
        1 |+| 2 shouldBe 3
      }

      {
        import IntMonoids.Multiplicative
        Monoid[Int] shouldBe IntMonoids.Multiplicative
        Semigroup[Int] shouldBe IntMonoids.Multiplicative

        import Monoid.ops._
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
        self =>
        class WithFilter[A](fa: F[A], p: A => Boolean) {
          def map[B](f: A => B): F[B] = self.map(filter(fa)(p))(f)
          def flatMap[B](f: A => F[B]): F[B] = self.flatMap(filter(fa)(p))(f)
          def withFilter(q: A => Boolean): WithFilter[A] = new WithFilter[A](fa, x => p(x) && q(x))
        }

        def withFilter[A](fa: F[A])(p: A => Boolean): WithFilter[A] = new WithFilter[A](fa, p)
        def filter[A](fa: F[A])(f: A => Boolean) =
          flatMap(fa)(a => if (f(a)) pure(a) else empty[A])
      }

      sealed trait Maybe[+A]
      case class Just[A](value: A) extends Maybe[A]
      case object Empty extends Maybe[Nothing]
      object Maybe {

        def just[A](a: A): Maybe[A] = Just(a)
        def empty[A]: Maybe[A] = Empty

        implicit val instance: MonadPlus[Maybe] = new MonadPlus[Maybe] {
          def pure[A](a: => A) = just(a)
          def empty[A] = Maybe.empty[A]
          def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa match {
            case Just(a) => f(a)
            case e @ Empty => Empty
          }
        }
      }

      import MonadPlus.ops._

      // We get the map function from Functor.Ops, which is the super-super-super class of MonadPlus.Ops
      Maybe.just(1) map ((_: Int) + 1) shouldBe Maybe.just(2)

      // We get >>= syntax as an alias for flatMap from the super-class of MonadPlus.Ops
      val recriprocal: Int => Maybe[Double] = x => if (x == 0) Maybe.empty else Maybe.just(1.0 / x)
      Maybe.just(1) >>= recriprocal

      // We get map from Functor.Ops, flatMap from Monad.Ops, and filter from MonadPlus.Ops
      def div(x: Maybe[Int], y: Maybe[Int]): Maybe[Double] = for {
        xx <- x
        yy <- y
        if (yy > 0)
      } yield xx.toDouble / yy

      div(Maybe.just(1), Maybe.just(2)) shouldBe Maybe.just(1.toDouble / 2)
      div(Maybe.just(1), Maybe.empty) shouldBe Maybe.empty
    }

    @typeclass trait Bifunctor[F[_, _]] {
      def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]
      def first[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, identity)
      def second[A, B, C](fab: F[A, B])(f: B => C): F[A, C] = bimap(fab)(identity, f)
    }

    import Bifunctor.ops._

    object TupleBifunctor {
      implicit val instance: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
        def bimap[A, B, C, D](fab: (A, B))(f: A => C, g: B => D): (C, D) = fab match {
          case (a, b) => (f(a), g(b))
        }
      }
    }

    @typeclass trait Trifunctor[F[_, _, _]] {
      def trimap[A, B, C, D, E, G](fabc: F[A, B, C])(f: A => D, g: B => E, h: C => G): F[D, E, G]
      def first[A, B, C, D](fabc: F[A, B, C])(f: A => D): F[D, B, C] = trimap(fabc)(f, identity, identity)
      def second[A, B, C, D](fabc: F[A, B, C])(f: B => D): F[A, D, C] = trimap(fabc)(identity, f, identity)
      def third[A, B, C, D](fabc: F[A, B, C])(f: C => D): F[A, B, D] = trimap(fabc)(identity, identity, f)
    }

    import Trifunctor.ops._

    object TupleTrifunctor {
      implicit val instance: Trifunctor[Tuple3] = new Trifunctor[Tuple3] {
        def trimap[A, B, C, D, E, G](fabc: (A, B, C))(f: A => D, g: B => E, h: C => G): (D, E, G) = fabc match {
          case (a, b, c) => (f(a), g(b), h(c))
        }
      }
    }

    @typeclass trait Strong[F[_, _]] {
      def first[A, B, C](fa: F[A, B]): F[(A, C), (B, C)]
    }

    object Function1Strong {
      implicit val instance: Strong[Function1] = new Strong[Function1] {
        def first[A, B, C](f: A => B): ((A, C)) => (B, C) = (ac: ((A, C))) =>  f(ac._1) -> ac._2
      }
    }

    import Strong.ops._

    val fab = (_: Int) => "World"

    "support type classes that are polymorphic over a binary type constructor" in {
      import TupleBifunctor._
      (("Hello", 42) second fab) shouldBe ("Hello", "World")
    }

    "support type classes that are polymorphic over a tertiary type constructor" in {
      import TupleTrifunctor._
      (("Hello", "World", 42) third fab) shouldBe ("Hello", "World", "World")
    }

    "strip type arguments from ops" in {
      import Function1Strong._
      (fab.first[String] apply (42 -> "Hello")) shouldBe ("World", "Hello")
    }

    "support using ops from unrelated type classes in the same scope" in {
      @typeclass trait Equal[A] {
        @op("=#=") def equal(x: A, y: A): Boolean
      }
      @typeclass trait Semigroup[A] {
        @op("|+|") def append(x: A, y: A): A
      }
      @typeclass trait Monoid[A] {
        def id: A
      }

      implicit val intInstance: Equal[Int] with Semigroup[Int] = new Equal[Int] with Semigroup[Int] {
        def equal(x: Int, y: Int) = x == y
        def append(x: Int, y: Int) = x + y
        def id: Int = 0
      }

      // We cannot import Equal.Ops and Semigroup.Ops because of the name clash
      // However, an alias for the implicit conversion is generated which allows direct import
      {
        import Equal.ops._, Semigroup.ops._
        (1 |+| 2) =#= (2 |+| 1)
      }

      // Alternatively, multiple type class ops can be combined in to a syntax object, which provides
      // a single import for all implicit conversions
      {
        object all extends Equal.ToEqualOps with Semigroup.ToSemigroupOps with Monoid.ToMonoidOps
        import all._
        (1 |+| 2) =#= (2 |+| 1)
      }
    }
  }
}

