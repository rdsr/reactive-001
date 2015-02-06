package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int ⇒
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("empty") = forAll { a: Int ⇒
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("meld") = forAll { (a: H, b: H) ⇒
    val h = meld(a, b)
    findMin(h) == Math.min(findMin(b), findMin(a))
  }

  property("sorted0") = forAll { h: H ⇒
    sorted(h)
  }

  property("sorted") = forAll { (a: H, b: H) ⇒
    sorted(meld(a, b))
  }

  property("no missing") = forAll { (a: H, b: H) ⇒
    val h = meld(a, b)
    asList(h).sorted == (asList(a) ++ asList(b)).sorted
  }

  property("deletion") = forAll { h: H ⇒
    sorted(deleteMin(h))
  }

  property("min insert") = forAll { h: H ⇒
    findMin(insert(Int.MinValue, h)) == Int.MinValue
  }

  lazy val genHeap: Gen[H] = for {
    k ← arbitrary[Int]
    h ← oneOf(value(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def sorted(h: H, pA: Int = Int.MinValue): Boolean =
    if (isEmpty(h)) true
    else {
      val a = findMin(h)
      if (a >= pA) sorted(deleteMin(h), a)
      else false
    }

  def size(h: H): Int =
    if (isEmpty(h)) 0
    else size(deleteMin(h)) + 1

  def asList(h: H): List[Int] =
    if (isEmpty(h)) List()
    else {
      val a = findMin(h)
      a :: asList(deleteMin(h))
    }
}
