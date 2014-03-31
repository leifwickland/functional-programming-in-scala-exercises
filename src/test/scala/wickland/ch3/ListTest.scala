package wickland
package ch3

import ListSpec._

class ListSpec extends BaseSpec {
  // Ex 1 is head work.

  val minPropertyTests = 10

  "Ex2: tail" should "be equal to the list without the head" in {
    forAll(minSuccessful(minPropertyTests)) { (list: List[Int]) =>
      val expected = list match {
        case Nil | Cons(_, Nil) => Nil
        case Cons(_, tail) => tail
      }
      List.tail(list) shouldBe expected
    }
  }

  "Ex3: setHead" should "replace the first element" in {
    forAll(minSuccessful(minPropertyTests)) { (list: List[Char], c: Char) =>
      val Cons(actualHead, actaulTail) = List.setHead(list)(c)
      actualHead shouldBe c
      actaulTail shouldBe List.tail(list)
    }
  }

  "Ex4: drop" should "remove a given number of elements from the head" in {
    forAll(minSuccessful(minPropertyTests)) { (list: List[Char], n: Int) =>
      var expected = list
      var i = n
      while (i > 0) {
        expected = List.tail(expected)
        i -= 1
      }
      List.drop(list, n) shouldBe expected
    }
  }

  "Ex5: dropWhile" should "remove elements from the head while a predicate holds" in {
    val p: Char => Boolean = { c: Char => c < 100 && c % 2 == 0 }
    forAll(minSuccessful(minPropertyTests)) { (list: List[Char]) =>
      var expected = list
      var matches = true
      while (matches) {
        list match {
          case Cons(head, tail) if (p(head)) => expected = tail
          case _ => matches = false
        }
      }
      List.dropWhile(list, p) shouldBe expected
    }
  }

  "Ex6: init" should "remove the last element" in {
    forAll(minSuccessful(minPropertyTests)) { (list: List[Int], i: Int) =>
      val newList = List.append(list, Cons(i, Nil))
      val actual = List.init(newList)
      actual shouldBe list
    }
  }

  // Ex7 is head work

  // Ex8 is head work

  "Ex9: length" should "calculate the list length" in {
    forAll(minSuccessful(minPropertyTests)) { (list: List[Int]) =>
      var temp = list
      var len = 0
      var done = false
      while (!done) {
        temp match {
          case Nil => done = true
          case Cons(_, tail) =>
            len += 1
            temp = tail
        }
      }
      List.length(list) shouldBe len
    }
  }

  "Ex10: foldLeft" should "not blow the stack" in {
    List.foldLeft(longList, 0)(_ + _) shouldBe 50015001
  }

  val longList: List[Int] = {
    var i = 10001
    var l: List[Int] = Nil
    while (i > 0) {
      l = Cons(i, l)
      i -= 1
    }
    l
  }
}

object ListSpec {
  import org.scalacheck._
  import org.scalacheck.Arbitrary.arbitrary

  implicit def arbList[T](implicit a: Arbitrary[T]): Arbitrary[ch3.List[T]] = Arbitrary {
    val maxLength = 20 // Just for speeding the tests up
    for {
      scalaList <- Gen.listOf[T](a.arbitrary)
      n <- arbitrary[Int]
      scalaList <- Gen.listOfN[T](20.min(n), a.arbitrary)
      myList = scalaList.reverse.foldLeft[List[T]](Nil) { case (acc, head) => Cons(head, acc) }
    } yield myList
  }
}
