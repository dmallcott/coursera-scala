package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.{forAll, propBoolean}

import scala.language.postfixOps

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap :

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      element <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(element, heap)
  )

  given Arbitrary[H] = Arbitrary(genHeap)

  // Useful for some tests
  lazy val genNonEmptyHeap: Gen[H] = for {
    h <- arbitrary[H]
    x <- arbitrary[Int]
  } yield if (isEmpty(h)) insert(x, h) else h

  // Code sample
  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap,
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("Bogus1") = forAll { (_: A) =>
    val h1 = insert(1, empty)
    val h2 = insert(2, h1)
    findMin(h2) == 1
  }

  // If you insert an element into an empty heap,
  // then delete the minimum, the resulting heap should be empty.
  property("Bogus2") = forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  // Deleting min should behave accordingly
  property("Bogus3") = forAll { (x: Int, y: Int) =>

    val min = if (x < y) x else y
    val max = if (x > y) x else y

    val h1 = insert(x, empty)
    val h2 = insert(y, h1)
    val h3 = deleteMin(h2)

    findMin(h3) == max
  }

  // Given any heap, you should get a sorted sequence of elements
  // when continually finding and deleting minima.
  // (Hint: recursion and helper functions are your friends.)
  property("Bogus4") = forAll { (xs: List[Int]) =>
    val h = insertAll(xs, empty)
    val ys = removeAll(h)
    xs.sorted == ys
  }

  // Finding a minimum of the melding of any two heaps
  // should return a minimum of one or the other.
  property("Bogus5") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    val min = if (h1Min < h2Min) h1Min else h2Min

    val h3 = meld(h1, h2)
    findMin(h3) == min
  }

  private def removeAll(heap: H): List[Int] =
    if (isEmpty(heap)) Nil
    else {
      val min = findMin(heap)
      val h = deleteMin(heap)
      min :: removeAll(h)
    }

  private def insertAll(xs: List[Int], heap: H): H = xs match {
    case Nil => empty
    case y :: ys =>
      insert(y, insertAll(ys, heap))
  }