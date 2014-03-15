package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  // insert any element into empty heap, minimum element of heap should equal element inserted
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  // insert two elements into empty heap, minimum element of heap should equal minimum of two elements
  property("min2") = forAll { a: Int =>
  	val h1 = insert(a, empty)
  	forAll { b: Int =>
  	  val h2 = insert(b, h1)
  	  if (a < b) {
  	    findMin(h2) == a
  	  }
  	  else {
  	    findMin(h2) == b
  	  }
  	}
  }

  // insert an element into empty heap, delete minimum element from heap, heap should be empty
  property("insdel") = forAll { a: Int =>
  	val h = insert(a, empty)
  	deleteMin(h) == empty
  }
  
  // meld two heaps, minimum of merged heap should equal minimum from one or the other
  property("meld") = forAll { (h1: H) =>
    forAll { (h2: H) =>
      val h = meld(h1, h2)
      findMin(h) == findMin(h1) || findMin(h) == findMin(h2)
    }
  }
  
  // merge two heaps; delete minimum element from one heap, insert the same element into the second heap, and merge
  // two merged heaps should be equal
  property("meld2") = forAll { (h1: H) =>
    forAll { (h2: H) => 
      val h3 = meld(deleteMin(h1), insert(findMin(h1), h2))
      val h4 = meld(h1, h2)
      isEqual(h3, h4)
    }
  }
  
  // helper function - determine if contents of two heaps are identical
  def isEqual(h1: H, h2: H) : Boolean = {
    if (isEmpty(h1) && isEmpty(h2)) true
    else if (findMin(h1) == findMin(h2)) isEqual(deleteMin(h1), deleteMin(h2))
    else false
  }
  
  lazy val genHeap: Gen[H] = for {
	 n <- arbitrary[Int]
	 h <- oneOf(value(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
