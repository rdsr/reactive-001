
import scala.util._
import scala.concurrent._
import ExecutionContext.Implicits.global

object repl {
	val x:Future[Unit] = future(() => {
		println("hello")
	})                                        //> x  : scala.concurrent.Future[Unit] = scala.concurrent.impl.Promise$DefaultPr
                                                  //| omise@54b24c03
	
	val p = Promise[String]()                 //> p  : scala.concurrent.Promise[String] = scala.concurrent.impl.Promise$Defaul
                                                  //| tPromise@3937bf4
  p.success("hello")                              //> res0: repl.p.type = scala.concurrent.impl.Promise$DefaultPromise@3937bf4
  p trySuccess "why"                              //> res1: Boolean = false
  p.future.isCompleted                            //> res2: Boolean = true
  
  Success("hello")                                //> res3: scala.util.Success[String] = Success(hello)
  
}
	