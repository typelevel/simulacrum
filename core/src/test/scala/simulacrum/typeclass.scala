package simulacrum

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

// NB: These imports are because the tests are compiled with `-Yno-imports`, to
//     ensure that simulacrum works in projects that use that flag.
import java.lang.String
import scala.{ Any, Nothing, Boolean, Either, Int, Left, Nil, Option, Right, Some }
import scala.Predef.{ ???, identity }
import scala.collection.immutable.List
import scala.util

/**
  * Semigroup description
  * @tparam T type T
  */
@typeclass trait Semigroup[T] {
  /**
    * append description
    * @param x param x
    * @param y param y
    * @return return value
    */
  @op("|+|", alias = true)
  def append(x: T, y: T): T
  def appendCurried(x: T)(y: T): T = append(x, y)
}

object Semigroup {
  implicit val semigroupInt: Semigroup[Int] = new Semigroup[Int] {
    def append(x: Int, y: Int) = x + y
  }
}

@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def as[A, B](fa: F[A], b: => B): F[B] = map(fa)(_ => b)
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  def foo[G[_], A](fga: F[G[A]]): G[F[A]] = ???
}

object Functor {
  implicit val functorList: Functor[List] = new Functor[List] {
    def map[A, B](fa: List[A])(f: A => B) = fa.map(f)
  }
}

class TypeClassTest extends AnyWordSpec with Matchers {

  "the @typeclass annotation" should {

    "support type classes that are polymorphic over a proper type," which {

      "generates an implicit summoning method in companion" in {
        Semigroup[Int] shouldBe Semigroup.semigroupInt
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

      "supports varargs in adapted methods" in {

        @typeclass trait Vargs[T] {
          @op("/:", true) def fold[T2](x: T, y: T2*)(f: (T, T2) => T): T
        }

        case class Sum(total: Int = 0) {
          def add(operand: Int): Sum = Sum(total + operand)
        }

        implicit val sumVargs: Vargs[Sum] = new Vargs[Sum] {
          def fold[T2](x: Sum, ys: T2*)(f: (Sum, T2) => Sum): Sum =
            ys.foldLeft(x) { (ds, t2) => f(ds, t2) }
        }

        import Vargs.ops._

        val sum = Sum()
        val ops = List(1, 2, 3, 4)

        sum.fold(ops: _*) { _ add _ } shouldBe Sum(10)
        sum./: ((ops ::: ops): _*) { _ add _ } shouldBe Sum(20)
      }

      "supports suppression of adapter methods" in {
        @typeclass trait Sg[A] {
          @noop def append(x: A, y: A): A
          @simulacrum.noop def foo(x: A, y: A): A = append(x, y)
        }
        implicit val sgInt: Sg[Int] = new Sg[Int] {
          def append(x: Int, y: Int) = x + y
        }

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
        implicit val qint: Qux[Int] = new Qux[Int] {
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

      "support importing only the non-inherited ops" in {
        @typeclass trait Foo[A] { def foo(a: A): A }
        @typeclass trait Bar[A] extends Foo[A] { def bar(a: A): A }

        import Bar.nonInheritedOps._
        implicit val intBar: Bar[Int] = new Bar[Int] {
          def foo(a: Int) = -a
          def bar(a: Int) = -a
        }
        5.bar shouldBe -5
        "5.foo" shouldNot compile
      }
    }

    "support type classes that are polymorphic over a type constructor," which {

      "generates an implicit summoning method in companion" in {
        Functor[List] shouldBe Functor.functorList
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
        implicit val listFoo: Foo[List] = new Foo[List] { def toEither[A](fa: List[A]) = if (fa.isEmpty) Left("empty") else Right(fa.head) }
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

      "supports type inference for syntax for operations where the instance type is constrained" in {
        @typeclass trait Ap[F[_]] {
          def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
        }

        object Ap {
          implicit val apOption: Ap[Option] = new Ap[Option] {
            def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = ff.flatMap(fa.map)
          }
        }

        import Ap.ops._
        val ff: Option[Int => String] = Some(_.toString)
        ff.ap(Some(0)) shouldBe Some("0")
      }
    }

    "support annotated companions" in {
      class exports extends scala.annotation.StaticAnnotation
      @typeclass trait Foo[F[_]]
      @exports object Foo
    }

    /** It'd be a lot better to parameterize all the tests over the variance
     *  of the type parameter so they can all be run again, but I"m not sure
     *  how to do it.
     */
    "support variant type classes" which {
      "are covariant" in {
        @typeclass trait Empty[+A] {
          def empty: A
        }
        @typeclass trait Empty1[+A] extends Empty[A] {
          def empty2[A1 >: A]: A1
        }
        @typeclass trait Empty2[A] extends Empty[A] {  // invariant
          def id(x: A): A
        }
      }
      "are contravariant" in {
        @typeclass trait Order[-A] {
          def cmp(x: A, y: A): Boolean
        }
        @typeclass trait Order1[-A] extends Order[A] {
          def cmp2[A1 <: A](x: A1, y: A1): Boolean
        }
        @typeclass trait Order2[A] extends Order[A] { // invariant
          def id(x: A): A
        }
      }
    }

    /** As above, it would pay to exercise these combinations as
     *  exhaustively as possible. Not that anyone has ever troubled
     *  themselves with doing that in the compiler itself.
     */
    "handle abstract type members (1)" in {
      case class Dingo(x: Int)
      var dingo: Dingo = null

      object typeClasses {
        @typeclass trait Bippoid[A] {
          type Bippy
          def append(x: A, y: A): A
          def secretId: Bippy
          def fn(x: Bippy): Int
        }
        @typeclass trait Foldable[F[_]] {
          def reduce[A](fa: F[A])(f: (A, A) => A): A
          def obtainDingo[A](fa: F[A])(implicit z: Bippoid[A]): (z.Bippy, A) = (z.secretId, reduce(fa)(z.append))
        }
      }
      import typeClasses._

      // NB: Having an explicit type here breaks the associated type usage.
      implicit val intBippoid: Bippoid[Int] { type Bippy = Dingo } =
        new Bippoid[Int] {
          type Bippy = Dingo
          def append(x: Int, y: Int) = x + y
          def secretId = Dingo(5)
          def fn(x: Bippy) = x.x
        }
      implicit val foldInstance: Foldable[List] = new Foldable[List] {
        def reduce[A](fa: List[A])(f: (A, A) => A): A = fa reduceLeft f
      }

      import Foldable.ops._

      // Making sure we can assign to the dingo var with a value
      // which has the abstract type from the Bippoid.
      val dep = List(1, 2, 3).obtainDingo
      dingo = dep._1
      dingo shouldBe Dingo(5)
      dingo = Dingo(10)
      intBippoid.fn(dingo) shouldBe 10
    }

    "handle abstract type members (2)" in {
      @typeclass trait Companion1[T] {
        type A
        def apply(a: A): T
        def unapply(t: T): Option[A]
      }
      object Foo extends Companion1[String] {
        type A = Int
        def apply(x: Int) = x.toString
        def unapply(s: String) = util.Try(java.lang.Integer.parseInt(s)).toOption
      }
      Foo(1) shouldBe "1"
      Foo.unapply("1") shouldBe Some(1)
    }

    "support dependent types in ops and summoner" in {
      @typeclass
      trait First[P] {
        type Out >: Nothing <: Any  // bounds are supported but not required
        def first(product: P): Out
      }
      object First {
        type Aux[T, Out0] = First[T] { type Out = Out0 }

        implicit def firstOfPair[A, B]: First.Aux[(A, B), A] = new First[(A, B)] {
          override type Out = A
          override def first(pair: (A, B)) = pair._1
        }
      }

      val p = (1, "xxx")
      import First.ops._
      val a = p.first
      val i: Int = a  // The compiler should know a has type Int

      val b = First[(Int,String)].first(p)
      val j: Int = b  // The compiler should know b has type Int
    }

    "support dependent type constructors in ops and summoner" in {
      import scala.collection.mutable
      @typeclass trait Mutable[S[_]] {
        type R[_]
        def toMutable[A](source: S[A]): R[A]
      }
      object Mutable {
        type Aux[S[_],R0[_]] = Mutable[S]{ type R[x] = R0[x] }
        implicit def converterImpl: Aux[List, mutable.ListBuffer] = new Mutable[List] {
          type R[x] = mutable.ListBuffer[x]
          def toMutable[A](s: List[A]) = {
            val buf = mutable.ListBuffer[A]()
            buf ++= s
            buf
          }
        }
      }

      import Mutable.ops._
      val a = List(1,2,3).toMutable
      val i: mutable.ListBuffer[Int] = a  // The compiler should know a has type mutable.ListBuffer[Int]

      val b = Mutable[List].toMutable(List(1,2,3))
      val j: mutable.ListBuffer[Int] = b  // The compiler should know b has type mutable.ListBuffer[Int]
    }

    "support lots of dependent types" in {
      @typeclass trait Foo[P] {
        type A
        type B
        type C
        type F[_,_,_]
        def a(p: P): A
        def b(p: P): B
        def c(p: P): C
        def make[X,Y,Z](x: X, y: Y, z: Z): F[X,Y,Z]
      }

      object Foo {
        type Aux[P,A0,B0,C0,F0[_,_,_]] = Foo[P] { type A=A0; type B=B0; type C=C0; type F[x,y,z] = F0[x,y,z] }

        implicit def foo[A0,B0,C0]: Foo.Aux[(A0,B0,C0),A0,B0,C0,scala.Tuple3] = new Foo[(A0,B0,C0)] {
          type A = A0
          type B = B0
          type C = C0
          type F[x,y,z] = (x,y,z)
          def a(p: (A0,B0,C0)) = p._1
          def b(p: (A0,B0,C0)) = p._2
          def c(p: (A0,B0,C0)) = p._3
          def make[X,Y,Z](x: X, y: Y, z: Z) = (x,y,z)
        }
      }

      import Foo.ops._

      val tuple: (Int,String,Option[Int]) = Foo[(Int,Int,Int)].make(1,"a",Some(2))

      val a: Int = tuple.a
      val b: String = tuple.b
      val c: Option[Int] = tuple.c
    }

    "generate universal traits by default" in {
      trait Foo[F[_]] extends Any with Functor[F]
    }
  }
}
