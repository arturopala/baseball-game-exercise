package baseball

import org.scalatest.{Matchers, WordSpec}

class BaseballGameSpec extends WordSpec with Matchers {

  "BaseballGame" should {

    "accept incoming event" when {
      "match time is zero or positive" in {
        BaseballGame.accept(Event(matchTime = 1, (3, 0), 0, 3)) shouldBe defined
      }
      "team number is 0 or 1" in {
        BaseballGame.accept(Event(1, (3, 0), whoScored = 0, 3)) shouldBe defined
        BaseballGame.accept(Event(1, (3, 0), whoScored = 1, 3)) shouldBe defined
      }
    }

    "reject incoming event" when {
      "match time is negative" in {
        BaseballGame.accept(Event(matchTime = -1, (3, 0), 0, 3)) shouldBe None
      }
      "team number is invalid" in {
        BaseballGame.accept(Event(1, (3, 0), whoScored = -1, 3)) shouldBe None
        BaseballGame.accept(Event(1, (3, 0), whoScored = 2, 3)) shouldBe None
        BaseballGame.accept(Event(1, (3, 0), whoScored = 3, 3)) shouldBe None
      }
    }

    "calculate sum of match scores" in {
      BaseballGame.MatchScore.sum((0, 0), (0, 0)) shouldBe (0, 0)
      BaseballGame.MatchScore.sum((3, 0), (0, 1)) shouldBe (3, 1)
      BaseballGame.MatchScore.sum((0, 3), (1, 0)) shouldBe (1, 3)
      BaseballGame.MatchScore.sum((1, 3), (2, 2)) shouldBe (3, 5)
    }

    "calculate delta of match scores" in {
      BaseballGame.MatchScore.delta((0, 0), (0, 0)) shouldBe (0, 0)
      BaseballGame.MatchScore.delta((3, 0), (0, 1)) shouldBe (3, 0)
      BaseballGame.MatchScore.delta((0, 3), (1, 0)) shouldBe (0, 3)
      BaseballGame.MatchScore.delta((1, 3), (2, 2)) shouldBe (0, 1)
    }

    "recalculate match score" when {
      "pointsScored for team 0 greater than zero" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 3)
        BaseballGame.recalculateMatchScore(event, previous) shouldBe Some(
          Event(matchTime = 1, currentMatchScore = (4, 0), whoScored = 0, pointsScored = 3))
      }
      "pointsScored for team 1 greater than zero" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 3)
        BaseballGame.recalculateMatchScore(event, previous) shouldBe Some(
          Event(matchTime = 1, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 3))
      }
      "pointsScored equals to zero" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 0)
        BaseballGame.recalculateMatchScore(event, previous) shouldBe None
      }
    }

    "recalculate points scored" when {
      "current match score delta shows that team 0 scored" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (4, 0), whoScored = 1, pointsScored = 0)
        BaseballGame.recalculatePointsScored(event, previous) shouldBe Some(
          Event(matchTime = 1, currentMatchScore = (4, 0), whoScored = 0, pointsScored = 3))
      }
      "current match score delta shows that team 1 scored" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 0)
        BaseballGame.recalculatePointsScored(event, previous) shouldBe Some(
          Event(matchTime = 1, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 3))
      }
      "current match score delta shows that both teams scored" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (2, 3), whoScored = 1, pointsScored = 0)
        BaseballGame.recalculatePointsScored(event, previous) shouldBe None
      }
      "current match score delta shows that none team scored" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 0)
        BaseballGame.recalculatePointsScored(event, previous) shouldBe None
      }
    }

    "normalize first event" when {
      "match score is inconsistent with scored points" in {
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 3)) shouldBe
          Some(Event(matchTime = 1, currentMatchScore = (3, 0), whoScored = 0, pointsScored = 3))
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (0, 1), whoScored = 0, pointsScored = 3)) shouldBe
          Some(Event(matchTime = 1, currentMatchScore = (3, 0), whoScored = 0, pointsScored = 3))
      }
      "only match score is reported" in {
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (3, 0), whoScored = 0, pointsScored = 0)) shouldBe
          Some(Event(matchTime = 1, currentMatchScore = (3, 0), whoScored = 0, pointsScored = 3))
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (0, 3), whoScored = 0, pointsScored = 0)) shouldBe
          Some(Event(matchTime = 1, currentMatchScore = (0, 3), whoScored = 1, pointsScored = 3))
      }
      "both match score and scored points are unknown" in {
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (0, 0), whoScored = 0, pointsScored = 0)) shouldBe
          None
      }
      "points scored are zero but match score shows points for both teams" in {
        BaseballGame.normalizeFirst(Event(matchTime = 1, currentMatchScore = (3, 2), whoScored = 0, pointsScored = 0)) shouldBe
          Some(Event(matchTime = 1, currentMatchScore = (3, 2), whoScored = 0, pointsScored = 0))
      }
    }

    "normalize inconsistent events" when {
      "first event's match score does not reflect scored points" in {
        val event = Event(matchTime = 1, currentMatchScore = (0, 0), whoScored = 0, pointsScored = 3)
        BaseballGame.normalize(event, None) shouldBe Some(
          Event(matchTime = 1, currentMatchScore = (3, 0), whoScored = 0, pointsScored = 3))
      }

      "current match score is not updated with regard to previous and scored points" in {
        val previous = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event = Event(matchTime = 2, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 3)
        BaseballGame.normalize(event, Some(previous)) shouldBe Some(
          Event(matchTime = 2, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 3))
      }
    }

    "properly report game state" when {
      "before receives any event" in {
        val game = new BaseballGame
        game.recent shouldBe None
        game.events() shouldBe empty
      }

      "after received first event" in {
        val game = new BaseballGame
        val event = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        game.receive(event)
        game.recent shouldBe Some(event)
        game.events() should contain.only(event)
      }

      "after received series of ordered and consistent events" in {
        val game = new BaseballGame
        val event1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event2 = Event(matchTime = 2, currentMatchScore = (1, 1), whoScored = 1, pointsScored = 1)
        val event3 = Event(matchTime = 3, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 2)
        game.receive(event1).receive(event2).receive(event3)
        game.recent shouldBe Some(event3)
        game.events() should contain.inOrder(event3, event2, event1)
        game.events(limit = Some(2)) should contain.inOrder(event3, event2)
      }

      "after received series of consistent but mixed order events" in {
        val game = new BaseballGame
        val event1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event2 = Event(matchTime = 2, currentMatchScore = (1, 1), whoScored = 1, pointsScored = 1)
        val event3 = Event(matchTime = 3, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 2)
        game.receive(event1).receive(event3).receive(event2)
        game.recent shouldBe Some(event3)
        game.events() should contain.inOrder(event3, event2, event1)
        game.events(limit = Some(2)) should contain.inOrder(event3, event2)
      }

      "after received series of consistent events containing duplicates" in {
        val game = new BaseballGame
        val event1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event2 = Event(matchTime = 2, currentMatchScore = (1, 1), whoScored = 1, pointsScored = 1)
        val event3 = Event(matchTime = 3, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 2)
        game.receive(event1).receive(event1).receive(event2).receive(event3).receive(event2)
        game.recent shouldBe Some(event3)
        game.events() should contain.inOrder(event3, event2, event1)
        game.events(limit = Some(2)) should contain.inOrder(event3, event2)
      }

      "after received series of ordered but inconsistent match score events" in {
        val game = new BaseballGame
        val event1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event2 = Event(matchTime = 2, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 1)
        val event3 = Event(matchTime = 3, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 2)

        game.receive(event1).receive(event2).receive(event3)

        val expected1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val expected2 = Event(matchTime = 2, currentMatchScore = (1, 1), whoScored = 1, pointsScored = 1)
        val expected3 = Event(matchTime = 3, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 2)

        game.recent shouldBe Some(expected3)
        game.events() should contain.inOrder(expected3, expected2, expected1)
        game.events(limit = Some(2)) should contain.inOrder(expected3, expected2)
      }

      "after received series of mixed order and inconsistent match score events" in {
        val game = new BaseballGame
        val event1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val event2 = Event(matchTime = 3, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 2)
        val event3 = Event(matchTime = 2, currentMatchScore = (1, 0), whoScored = 1, pointsScored = 1)

        game.receive(event1).receive(event2).receive(event3)

        val expected1 = Event(matchTime = 1, currentMatchScore = (1, 0), whoScored = 0, pointsScored = 1)
        val expected2 = Event(matchTime = 2, currentMatchScore = (1, 1), whoScored = 1, pointsScored = 1)
        val expected3 = Event(matchTime = 3, currentMatchScore = (1, 3), whoScored = 1, pointsScored = 2)

        game.recent shouldBe Some(expected3)
        game.events() should contain.inOrder(expected3, expected2, expected1)
        game.events(limit = Some(2)) should contain.inOrder(expected3, expected2)
      }
    }

    "process external supplier events" when {
      "stream of events is consistent" in {
        val game = new BaseballGame
        val events = Seq(0x801002, 0xf81016, 0x1d8102f, 0x248202a, 0x2e0203e, 0x348204e, 0x3b8384b, 0x468385e,
          0x5304059, 0x640406e, 0x6d8506a, 0x760606a, 0x838607e, 0x8e8707a, 0x930708e, 0x9f0709e, 0xad070a5, 0xb7880a2,
          0xbf880b6, 0xc9080c6, 0xd2090c2, 0xdd090d6, 0xed0a8d3, 0xf98a8e6, 0x10a8b8e2, 0x1178b8ed, 0x1228c8ea,
          0x12b0d8ea)
        events.foreach(event => BaseballGame.receiveFromExternalSupplier(game)(event))
        game.recent shouldBe Some(BaseballEventSupplier.decode(events.last))
        game.events() should contain.theSameElementsInOrderAs(events.reverse.map(BaseballEventSupplier.decode))
      }

      "stream of events is inconsistent" in {
        val game = new BaseballGame
        val events = Seq(0x781002, 0xe01016, 0x1081014, 0x1e0102f, 0x258202a, 0x308203e, 0x388204e, 0x388204e,
          0x3d0384b, 0x478385e, 0x618406e, 0x5404059, 0x6b8506a, 0x750706c, 0x7d8507e, 0x938608e, 0x8b8607a, 0xa10609e,
          0xb8870a2, 0xc4870b6, 0xcc070c6, 0x2ee74753, 0xd5080c2, 0xdf080d6, 0xe4098d3, 0xec098f6, 0xfc8a8e2,
          0x10a8a8ed, 0x1180b8ea, 0x1218c8ea)
        events.foreach(event => BaseballGame.receiveFromExternalSupplier(game)(event))
        game.recent shouldBe Some(Event(1500, (32, 28), 0, 3))
      }

    }
  }

}
