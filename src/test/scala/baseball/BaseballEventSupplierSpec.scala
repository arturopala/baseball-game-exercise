package baseball

import org.scalatest.{Matchers, WordSpec}

class BaseballEventSupplierSpec extends WordSpec with Matchers {

  "BaseballEventSupplier" should {
    "decode incoming event" in {
      BaseballEventSupplier.decode(0x0) shouldBe Event(0, (0, 0), 0, 0)
      BaseballEventSupplier.decode(0x781002) shouldBe Event(15, (2, 0), 0, 2)
      BaseballEventSupplier.decode(0xf0101f) shouldBe Event(30, (2, 3), 1, 3)
      BaseballEventSupplier.decode(0x1310c8a1) shouldBe Event(10 * 60 + 10, (25, 20), 0, 1)
      BaseballEventSupplier.decode(0x29f981a2) shouldBe Event(22 * 60 + 23, (48, 52), 0, 2)
      BaseballEventSupplier.decode(0x48332327) shouldBe Event(38 * 60 + 30, (100, 100), 1, 3)
    }

    "encode an event" in {
      BaseballEventSupplier.encode(Event(0, (0, 0), 0, 0)) shouldBe 0x0
      BaseballEventSupplier.encode(Event(15, (2, 0), 0, 2)) shouldBe 0x781002
      BaseballEventSupplier.encode(Event(30, (2, 3), 1, 3)) shouldBe 0xf0101f
      BaseballEventSupplier.encode(Event(10 * 60 + 10, (25, 20), 0, 1)) shouldBe 0x1310c8a1
      BaseballEventSupplier.encode(Event(22 * 60 + 23, (48, 52), 0, 2)) shouldBe 0x29f981a2
      BaseballEventSupplier.encode(Event(38 * 60 + 30, (100, 100), 1, 3)) shouldBe 0x48332327
    }
  }

}
