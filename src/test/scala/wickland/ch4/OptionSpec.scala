package wickland
package ch4

class OptionSpec extends BaseSpec {
  behavior of "Exercise1: None"
  it should "map() to None without calling f" in {
    None.map(_ => ???) shouldBe None
  }
  it should "flatMap() to None without calling f" in {
    None.flatMap(_ => ???) shouldBe None
  }
  it should "getOrElse() to the default" in {
    None.getOrElse("default") shouldBe "default"
  }
  it should "orElse() to the default" in {
    None.orElse(Some("default")) shouldBe Some("default")
  }
  it should "filter() to None without calling f" in {
    None.filter(_ => ???) shouldBe None
  }

  behavior of "Exercise1: Some"
  it should "map() to the result of f" in {
    Some(3).map(_ * 4) shouldBe Some(12)
  }
  it should "flatMap() to the result of f" in {
    Some(3).flatMap(x => Some(x * 4)) shouldBe Some(12)
  }
  it should "getOrElse() its value without evaluating the default" in {
    Some(3).getOrElse(???) shouldBe 3
  }
  it should "orElse() itself without evaluating the default" in {
    Some(3).orElse(???) shouldBe Some(3)
  }
  it should "filter() to itself if the predicate is true" in {
    Some(3).filter(_ == 3) shouldBe Some(3)
  }
  it should "filter() to None if the predicate is false" in {
    Some(3).filter(_ != 3) shouldBe None
  }
}
