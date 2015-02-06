object week1 {
  abstract class json
  case class jseq(js: List[json]) extends json
  case class jobj(bindings: Map[String, json])
  case class jstr(s: String) extends json
  case class jnum(n: Double) extends json
  case class jnil extends json

  val f = new scala.Function1[String, json] {
    def apply(s: String): json = jstr(s)
  }                                               //> f  : String => week1.json = <function1>
  val z = f("ratan")                              //> z  : week1.json = jstr(ratan)
  val y = "ratan"                                 //> y  : String = ratan

  val x: PartialFunction[String, String] = { case "ping" ⇒ "pong" }
                                                  //> x  : PartialFunction[String,String] = <function1>
  x.isDefinedAt("ping")                           //> res0: Boolean = true

  trait Generator[+T] {
    self ⇒

    def generate: T

    def map[S](f: T ⇒ S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T ⇒ Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate: Int = rand.nextInt
  }                                               //> integers  : week1.Generator[Int]{val rand: java.util.Random} = week1$$anonfu
                                                  //| n$main$1$$anon$4@5559fd10

  integers.generate                               //> res1: Int = -667378514

  val booleans = for (x ← integers) yield x > 0   //> booleans  : week1.Generator[Boolean] = week1$$anonfun$main$1$Generator$1$$an
                                                  //| on$2@499977e3

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x ← t
    y ← u
  } yield (x, y)                                  //> pairs: [T, U](t: week1.Generator[T], u: week1.Generator[U])week1.Generator[
                                                  //| (T, U)]

  trait Tree
  case class Inner(left: Tree, right: Tree) extends Tree
  case class Leaf(x: Int) extends Tree

  def trees: Generator[Tree] = for {
    leaf ← booleans
    tree ← if (leaf) leaves else inners
  } yield tree                                    //> trees: => week1.Generator[week1.Tree]

  def leaves: Generator[Leaf] = for {
    x ← integers
  } yield Leaf(x)                                 //> leaves: => week1.Generator[week1.Leaf]

  def inners: Generator[Inner] = for {
    x ← trees
    y ← trees
  } yield Inner(x, y)                             //> inners: => week1.Generator[week1.Inner]

  trees.generate                                  //> res2: week1.Tree = Inner(Leaf(-1165290264),Leaf(-1764674819))

  def test[T](g: Generator[T], nTimes: Int = 100)(test: T ⇒ Boolean): Unit = {
    for (i ← 0 until nTimes) {
      val v = g.generate
      assert(test(v), "Test failed for " + v)
    }
    println("Passed " + nTimes + " tests!")
  }                                               //> test: [T](g: week1.Generator[T], nTimes: Int)(test: T => Boolean)Unit
  
  def fn(z: (Int, Int)): Boolean = (z._1 > z._2 || z._1 <= z._2)
                                                  //> fn: (z: (Int, Int))Boolean
  test(pairs(integers, integers)) { fn }          //> Passed 100 tests!
}