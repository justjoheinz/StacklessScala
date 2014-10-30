package withscalaz

import scalaz._
import Id._
import Free._
import Trampoline._

object FreeWithScalaz extends App {
  sealed trait Directive[+Next] 
  case class GoLeft() extends Directive[Unit]
  case class GoRight() extends Directive[Unit]
  case class Finished() extends Directive[Unit]
  
  type Command[A] = FreeC[Directive,A]   
  
  def left : Command[Unit] = Free.liftFC(GoLeft())
  def right : Command[Unit] = Free.liftFC(GoRight()) 
  def finished : Command[Unit] = Free.liftFC(Finished())
  
  def fac(i: BigDecimal) : Trampoline[BigDecimal] = {
    import Trampoline._
    if (i == 0) done(1) else for {
      facM1 <- suspend(fac(i-1))
    } yield (i * facM1)
  }
  
  def move : Command[Unit] = {
    for {
      _ <- left
      _ <- right
      _ <- right
      _ <- finished
    } yield ()
  }

  object CmdInterpreter extends (Directive ~> Id) {
    def apply[A](cmd: Directive[A]) : Id[A] = {
      cmd match {
      case GoLeft()  =>
        println("go left")
        ()
      case GoRight()  =>
        println("go right")
        ()
      case Finished() => println("finished"); 
    }
    }
  }
 
  println(s"Fac: ${fac(20000).run}")
  Free.runFC(move)(CmdInterpreter)
  
}