package stackless
import scala.util._

trait Functor[F[_]] {
  def map[A, B](m: F[A])(f: A => B): F[B]
}

object Main extends App {

  object step1_basicrecursion {
    def fac(i: BigDecimal): BigDecimal = if (i == 0) 1 else i * fac(i - 1)

    // work arounds available
    // such as tail recursion available, but not always possible, or lack of support by JVM
  }

  object step2_nomonad {

    trait Trampoline[A] {
      @scala.annotation.tailrec
      final def run(): A = this match {
        case Done(k) => k
        case More(k) => k().run
      }
    }
    case class Done[A](k: A) extends Trampoline[A]
    case class More[A](k: () => Trampoline[A]) extends Trampoline[A]

    // Meeh, does not work -- need functor (and monad)
    //    def recursionFail(i: BigDecimal): Trampoline[BigDecimal] = 
    //      if (i == 0) Done(1) else i * More(() => i * recursionFail(i - 1))

    def fac(i: BigDecimal): BigDecimal = {
      def fac_internal(i: BigDecimal, result: BigDecimal): Trampoline[BigDecimal] = {
        if (i == 0) Done(result) else More(() => fac_internal(i - 1, result * i))
      }

      fac_internal(i, 1).run
    }
  }

  // False monad implementation
  object step3_falseMonad {
    trait Trampoline[A] {
      @scala.annotation.tailrec
      final def run(): A = this match {
        case Done(k) => k
        case More(k) => k().run
      }

      // D'Oh - run is basically a non tail recursion
      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = {
        More[B](() => f(run))
      }

      def map[B](f: A => B): Trampoline[B] = {
        flatMap(a => Done(f(a)))
      }
    }
    case class Done[A](k: A) extends Trampoline[A]
    case class More[A](k: () => Trampoline[A]) extends Trampoline[A]

    def fac(i: BigDecimal): Trampoline[BigDecimal] =
      if (i == 0) Done(1) else {
        for {
          facM1 <- More(() => fac(i - 1))
        } yield i * facM1
      }

  }

  //  Monad implementation
  object step4_monad {
    sealed trait Trampoline[+A] {
      @scala.annotation.tailrec
      final def run(): A = resume match {
        case Right(k) => k
        case Left(k)  => k().run
      }

      @scala.annotation.tailrec
      final def resume: Either[() => Trampoline[A], A] =
        this match {
          case Done(v) => Right(v)
          case More(k) => Left(k)
          case FlatMap(a, f) => a match {
            case Done(v)       => f(v).resume
            case More(k)       => Left(() => k() flatMap f)
            case FlatMap(b, g) => (b flatMap ((x: Any) => g(x) flatMap f): Trampoline[A]).resume
          }
        }

      def flatMap[B](f: A => Trampoline[B]): Trampoline[B] =
        this match {
          case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
          case x             => FlatMap(x, f)
        }

      final def map[B](f: A => B): Trampoline[B] = {
        flatMap(a => Done(f(a)))
      }
      private case class FlatMap[A, +B](sub: Trampoline[A], k: A => Trampoline[B]) extends Trampoline[B]
    }

    case class Done[+A](k: A) extends Trampoline[A]
    case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

    implicit def step[A](a: => A): Trampoline[A] = More(() => Done(a))

    def funnyProg: Trampoline[Unit] = {
      import scala.io.StdIn._
      for {
        _ <- print("What's you name? ")
        name <- step(readLine()) // ??? 
        _ <- println(s"Hello $name")
      } yield ()
    }

    // see 4.4
    def fac(i: BigDecimal): Trampoline[BigDecimal] = {
      if (i == 0) Done(1) else {
        for {
          facM1 <- More(() => fac(i - 1))
        } yield i * facM1
      }
    }
  }

  object step5_free {

    implicit def functorFunction0: Functor[Function0] = new Functor[Function0] {
      def map[A, B](m: Function0[A])(f: A => B): Function0[B] = () => f(m())
    }

    sealed trait Free[S[+_], +A] {

      final def go[A2 >: A](f: S[Free[S, A2]] => Free[S, A2])(implicit S: Functor[S]): A2 = {
        @scala.annotation.tailrec
        def go2(t: Free[S, A2]): A2 = t.resume match {
          case Left(s)  => go2(f(s))
          case Right(r) => r
        }
        go2(this)
      }

      def run[A2 >: A](implicit ev: Free[S, A2] =:= Trampoline[A2]): A2 =
        ev(this).go(_())

      final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] =
        this match {
          case Done(a) => Right(a)
          case More(k) => Left(k)
          case FlatMap(a, f) => a match {
            case Done(a) => f(a).resume
            case More(k) =>
              Left(S.map(k)(_ flatMap f))
            case FlatMap(b, g) => b.flatMap((x: Any) => g(x) flatMap f).resume
          }
        }

      def flatMap[B](f: A => Free[S, B]): Free[S, B] =
        this match {
          case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
          case x             => FlatMap(x, f)
        }

      final def map[B](f: A => B): Free[S, B] = {
        flatMap(a => Done(f(a)))
      }

      private case class FlatMap[S[+_], A, +B](
        a: Free[S, A],
        f: A => Free[S, B]) extends Free[S, B]
    }
    case class Done[S[+_], +A](a: A) extends Free[S, A]
    case class More[S[+_], +A](k: S[Free[S, A]]) extends Free[S, A]

    type Trampoline[A] = Free[Function0, A]

    def fac(i: BigDecimal): Trampoline[BigDecimal] = {
      if (i == 0) Done(1) else {
        for {
          facM1 <- More(() => fac(i - 1))
        } yield i * facM1
      }
    }
  }

  object step6_robot {
    import step5_free._
    
    sealed trait Directive[+Next]
    case class GoLeft[Next](next: Next) extends Directive[Next]
    case class GoRight[Next](next: Next) extends Directive[Next]
    case class Finished() extends Directive[Nothing]

    implicit def directiveFunctor[A]: Functor[Directive] = new Functor[Directive] {
      def map[A, B](m: Directive[A])(f: A => B): Directive[B] = {
        m match {
          case l: GoLeft[A]  => GoLeft(f(l.next))
          case r: GoRight[A] => GoRight(f(r.next))
          case Finished()    => Finished()
        }
      }
    }

    type Command[A] = Free[Directive, A]
    def liftF[S[+_], R](command: S[R])(implicit F: Functor[S]): Free[S, R] = More[S, R](F.map(command) { Done[S, R](_) })

    def left: Command[Unit] = liftF[Directive, Unit](GoLeft(()))
    def right: Command[Unit] = liftF[Directive, Unit](GoRight(()))
    def finished: Command[Unit] = liftF[Directive, Unit](Finished())

    def move(): step5_free.Free[Directive, Unit] = {
      for {
        _ <- left
        _ <- right
        _ <- finished
      } yield ()
    }

  }

  println(step1_basicrecursion.fac(6))
  println(step2_nomonad.fac(20000))
  // run with large number => 6
  println(step3_falseMonad.fac(6).run)
  println(step4_monad.fac(20000).run)
  // println(step4_monad.funnyProg.run)

  import step5_free._
  println(step5_free.fac(20000))
  println(step5_free.fac(20000).go(step => step()))
  println(step5_free.fac(20000).run)

  println(step6_robot.move().go { cmd =>
    import step6_robot._
    cmd match {
      case l: GoLeft[_]   =>
        println("go left"); l.next
      case r: GoRight[_]  =>
        println("go right"); r.next
      case f: Finished => println("finished"); Done(())
    }
  })

}