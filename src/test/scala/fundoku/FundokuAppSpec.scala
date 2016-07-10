package fundoku

import org.scalatest.{Matchers, WordSpecLike}

class ActivitySpec extends WordSpecLike with Matchers {

  import Activity._

  "addAndWrap" should {
    "not wrap" in {
      addAndWrap(5, 2, 10) shouldBe 7
    }
    "wrap" in {
      addAndWrap(9, 2, 10) shouldBe 1
    }
  }

  "indicesToUpdate" should {
    "not wrap around end" in {
      indicesToUpdate(0, 3, 10) shouldBe (false, Seq(0, 0, 3, 10))
    }
    "wrap around end" in {
      indicesToUpdate(8, 3, 10) shouldBe (true, Seq(0, 1, 8, 10))
      indicesToUpdate(8, 4, 10) shouldBe (true, Seq(0, 2, 8, 10))
      indicesToUpdate(9, 3, 10) shouldBe (true, Seq(0, 2, 9, 10))
    }
  }

  "updateActivity" should {
    "update correctly at start" in {
      val (nextI, activity) = updateActivity(2, 10)(0, Array.fill(4)(Activity()))
      nextI shouldBe 1
      activity shouldBe Seq(
        Activity(10),
        Activity(10),
        Activity(0),
        Activity(0)
      )
    }
    "update correctly when wrapping around end" in {
      val (nextI, activity) = updateActivity(2, 10)(3, Array.fill(4)(Activity()))
      nextI shouldBe 0
      activity shouldBe Seq(
        Activity(10),
        Activity(0),
        Activity(0),
        Activity(10)
      )
    }
  }
}
