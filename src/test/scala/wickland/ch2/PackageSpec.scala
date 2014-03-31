package wickland
package ch2

class PackageSpec extends BaseSpec {
  behavior of "Ex1: fibonocci"
  it should "do easy" in {
    fib(14) shouldBe 377
  }
  it should "do large signed 64-bit" in {
    fib(92) shouldBe 7540113804746346429L
  }

  behavior of "Ex2: isSorted"
  val intGt: (Int, Int) => Boolean = (a, b) => a < b
  it should "find sorted" in {
    isSorted((1 to 20).toArray)(intGt) shouldBe true
  }
  it should "find unsorted" in {
    val array = (1 to 20).toArray
    array(0) = 3
    isSorted(array)(intGt) shouldBe false
  }

  val plus: (Int, Int) => Int = (_ + _)
  "Ex3: partial1" should "compose a new function" in {
    val plus4 = partial1(4, plus)
    plus4(5) shouldBe (9)
  }

  "Ex4: curry" should "decompose a function" in {
    val curriedPlus = curry(plus)
    val plus5 = curriedPlus(5)
    plus5(6) shouldBe (11)
  }

  "Ex5: uncurry" should "reverse curry" in {
    val curriedPlus: Int => Int => Int = curry(plus)
    val uncurriedPlus: (Int, Int) => Int = uncurry(curriedPlus)
    forAll { (a: Int, b: Int) =>
      plus(a, b) shouldBe uncurriedPlus(a, b)
    }
  }

  "Ex6: compose" should "create a function which calls f and g in order" in {
    forAll { (f: Function1[Int, Double], g: Function1[String, Int], s: String) =>
      val composed = compose(f, g)
      composed(s) shouldBe f(g(s))
    }
  }

  // Chapter 2 from MEAP v8
  //  behavior of "Ex2: absolute"
  //  it should "not modify positive values" in {
  //    absolute(identity)(10) shouldBe 10
  //  }
  //  it should "make negative values positive" in {
  //    absolute(identity)(-10) shouldBe 10
  //  }
  //
  //
  //  behavior of "Ex3: absoluteP"
  //  val times1: Double => Double = (_ * 1)
  //  val absTimes1 = absoluteP(times1)
  //  it should "not modify positive values" in {
  //    absTimes1(10.0) shouldBe 10.0
  //  }
  //  it should "make negative values positive" in {
  //    absTimes1(-10.0) shouldBe 10.0
  //  }
  //
  //  behavior of "Ex4: divisibleBy"
  //  val divisibleBy3 = divisibleBy(3)
  //  it should "say 12 is divisibleBy 3" in {
  //    divisibleBy3(12) shouldBe true
  //  }
  //  it should "say 13 is not divisibleBy 3" in {
  //    divisibleBy3(13) shouldBe false
  //  }
}
