package baseball

import org.scalatest.{Matchers, WordSpec}

class EventListSpec extends WordSpec with Matchers {

  val reconcileFx: (Int, Option[Int]) => Some[Int] = (e, _) => Some(e)

  "EventList" should {

    "not fail if empty" in {
      val list = new EventList[Int](reconcileFx)
      list.recent shouldBe None
      list.events() shouldBe empty
    }

    "receive first event" in {
      val list = new EventList[Int](reconcileFx)
      list.receive(1)
      list.recent shouldBe Some(1)
      list.events() should contain.only(1)
    }

    "receive multiple ordered events" in {
      val list = new EventList[Int](reconcileFx)
      list
        .receive(1)
        .receive(2)
        .receive(3)
      list.recent shouldBe Some(3)
      list.events() should contain.inOrder(3, 2, 1)
      list.events(limit = Some(2)) should contain.inOrder(3, 2)
    }

    "receive multiple mixed order events" in {
      val list = new EventList[Int](reconcileFx)
      list
        .receive(1)
        .receive(4)
        .receive(3)
        .receive(4)
        .receive(2)
        .receive(1)
      list.recent shouldBe Some(4)
      list.events() should contain.inOrder(4, 3, 2, 1)
    }

    "consider ordering of events" in {
      val list = new EventList[Int](reconcileFx)(Ordering.fromLessThan[Int]((a, b) => b <= a))
      list
        .receive(1)
        .receive(4)
        .receive(3)
        .receive(4)
        .receive(2)
        .receive(1)
      list.recent shouldBe Some(1)
      list.events() should contain.inOrder(1, 2, 3, 4)
    }

    "replace element when normalize returns Some" in {
      val list = new EventList[Int]((e, _) => if (e == 2) Some(9) else Some(e))
      list
        .receive(1)
        .receive(4)
        .receive(3)
        .receive(4)
        .receive(2)
        .receive(1)
      list.recent shouldBe Some(4)
      list.events() should contain.inOrder(4, 3, 9, 1)
    }

    "omit element when normalize returns None" in {
      val list = new EventList[Int]((e, _) => if (e == 2) None else Some(e))
      list
        .receive(1)
        .receive(4)
        .receive(3)
        .receive(4)
        .receive(2)
        .receive(1)
      list.recent shouldBe Some(4)
      list.events() should contain.inOrder(4, 3, 1)
    }
  }

}
