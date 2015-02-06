package week1

import scala.util.control.NonFatal

object monads {
	// data structures with map and flatMap with some algebraic laws
	// are called monads
	
	trait M[T] {
		def flatMap[U](f: T => M[U]): M[U]
	  def unit[T](x: T): M[T]
	}
	
	/* some monads
	List is a monad with unit(x) = List(x)
	Option is a monad with unit(x) = Some(x)
	Generator is a monad with unit(x) = Single(x)
	*/
	
	
	// Option monad
	abstract class Option[+T] {
		def flatMap[U](f: T => Option[U]): Option[U] = this match {
			case Some(x) => f(x)
			case None => None
		}
	}
	
	case class Some[+T](x:T) extends Option[T]
	case object None extends Option[Nothing]
	
	
	// another type Try
	abstract class Try[+T] {
		def map[U](f: T => U): Try[U] = this match {
			case Success(x) => Try(f(x))
			case fail: Failure => fail
		}
		
		def flatMap[U](f: T => Try[U]): Try[U] = this match {
			case Success(x) => try f(x) catch { case NonFatal(e) => Failure(e) }
			case fail: Failure => fail
		}
	}
	
	case class Success[+T](x: T)     extends Try[T]
	case class Failure(e: Throwable) extends Try[Nothing]
	
	object Try {
		def apply[T](expr: => T): Try[T] =
			try Success(expr)
			catch {
				case NonFatal(e) => Failure(e)
			}
	}
	
	for {
		x <- Try(1/2)
		y <- Try(2/0)
	} yield (x, y)                            //> res0: week1.monads.Try[(Int, Int)] = Failure(java.lang.ArithmeticException:
                                                  //|  / by zero)
	
	Try(1) flatMap(x => for (y <- Try(2)) yield (x, y))
                                                  //> res1: week1.monads.Try[(Int, Int)] = Success((1,2))
  Try(1) flatMap(x => (Try(2) map (y => (x, y)))) //> res2: week1.monads.Try[(Int, Int)] = Success((1,2))
}