/*
abstract class Try[+T] {
	// higher order fns to manipulate Try[T]
	def flatMap[S](f: T => Try[S]): Try[S]
	// Todo: check
	def flatten[U <: Try[T]]: Try[U]
	def map[S](f: T => S): Try[S] = this match {
		case Success(e) => Try(f(e))
	}
	def filter(f: T => Boolean): Try[T]
	// Todo check
	def recoverWith(f: PartialFunction[Throwable, Try[T]]): Try[T]
}

object Try {
	def apply[T](r: =>T): Try[T] = {
		try { Success(r) }
		catch { case t: Throwable => Failure(t)	}
	}
}

case class Success[+T](e: T) extends Try[T]
case class Failure[+T](t: Throwable) extends Try[Nothing]
*/

trait Coin
trait Treasure

object week3 {
	trait Adventure {
		def collectCoins(): Try[List[	Coin]]
		def buyTreasure(coins: List[Coin]): Try[Treasure]
	}
	
	val a: Adventure = null
	val coins = a.collectCoins
	val treasure = coins match {
		case Success(e) => a.buyTreasure(e)
		case failure @ Failure(t) => failure
	}
		
	// Monads
	
	val treasure2 = a.collectCoins.flatMap(coins => {
		a.buyTreasure(coins)
	})
	
	// using for comprehension
	val treasure3 = for {
		coins <- a.collectCoins
		treasure <- a.buyTreasure(coins)
	} yield treasure
	
	
	import scala.concurrent._
	trait Socket {
		def readFromMemory(): Future[Array[Byte]]
		def sendToEurope(packet: Array[Byte]): Future[Array[Byte]]
	}
	
	import ExecutionContext.Implicits.global
	import scala.util._
	// Future[T]. A Monad that handles exceptions and latency
	val socket: Socket = null
	val packet = socket.readFromMemory
	val confirmation = packet onComplete {
		case Success(v) => socket.sendToEurope(v)
		case failure @ Failure(t) => failure
	}
	
	val packet2 = socket.readFromMemory
	packet2 onComplete {
		case Success(p) => {
			// do everything in the success clause
			// Sphagetti code!
			val confirmation = socket.sendToEurope(p)
		}
		case Failure(p) => {}
	}
	
	// using flatmap
	val packet3 = socket.readFromMemory
	val confirmation3: Future[Array[Byte]] =
	packet3 flatMap {
		p => socket.sendToEurope(p)
	}
}