package wickland.ch4

class TraverseSpec extends wickland.BaseSpec {
  behavior of "EX4: sequence"
  it should "flatten a list of all defined values" in {
    sequence(someList) shouldBe Some(list)
  }
  it should "convert a list containing None to None" in {
    val listContainingNone = List(Some(1), None)
    sequence(listContainingNone) shouldBe None
  }

  behavior of "EX5: traverse"
  it should "flatten and map" in {
    traverse(list)(x => Some(x.toString)) shouldBe Some(list.map(_.toString))
  }
  it should "return None if there's a None" in {
    val result = traverse(list) {
      case x if x % 2 == 0 => None
      case x => Some(x.toString)
    }
    result shouldBe None
  }

  behavior of "EX5: traverseTailRec"
  it should "flatten and map" in {
    traverseTailRec(list)(x => Some(x.toString)) shouldBe Some(list.map(_.toString))
  }
  it should "return None if there's a None" in {
    val result = traverseTailRec(list) {
      case x if x % 2 == 0 => None
      case x => Some(x.toString)
    }
    result shouldBe None
  }

  val list = List(1, 2, 3)
  val someList = list.map(Some.apply)
}
