package wickland.ch4

class PackageSpec extends wickland.BaseSpec {
  "EX2: mean" should "correctly calculate a simple example" in {
    mean(Seq(1, 2, 3)) shouldBe Some(2.0)
  }

  behavior of "EX2: variance"
  it should "return None for an empty Seq" in {
    variance(Nil) shouldBe None
  }
  it should "correctly calculate a simple example" in {
    // From http://www.mathsisfun.com/data/standard-deviation.html
    variance(Seq(600, 470, 170, 430, 300)) shouldBe Some(21704.0)
  }
  it should "return None given busted inputs" in {
    variance(Seq(Double.NegativeInfinity, Double.NaN, Double.PositiveInfinity)) shouldBe None
  }

  behavior of "EX3: map2"
  val times: (Int, Int) => Int = { _ * _ }
  it should "return None if the first argument is None" in {
    map2(None, Some(1))(times) shouldBe None
  }
  it should "return None if the second argument is None" in {
    map2(Some(1), None)(times) shouldBe None
  }
  it should "evaluate f if both values are present" in {
    map2(Some(3), Some(4))(times) shouldBe Some(12)
  }
}
