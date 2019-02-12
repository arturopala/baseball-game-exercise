package baseball

class BaseballGame {

  private val game = new EventList[Event](BaseballGame.normalize)

  /** Receives new event and updates game state */
  def receive(event: Event): BaseballGame = {
    game.receive(event)
    this
  }

  /** Returns game state as a recent event */
  def recent: Option[Event] = game.recent

  /** Returns list of events, ordered by match time descending */
  def events(limit: Option[Int] = None): List[Event] = game.events(limit)

}

object BaseballGame {

  def receiveFromExternalSupplier(game: BaseballGame)(event: Int): BaseballGame =
    game.receive(BaseballEventSupplier.decode(event))

  def accept(event: Event): Option[Event] =
    if (event.matchTime >= 0 && (event.whoScored == 0 || event.whoScored == 1))
      Some(event)
    else
      None

  def normalizeFirst(event: Event): Option[Event] = {
    val previous = event.copy(currentMatchScore = (0, 0))
    recalculateMatchScore(event, previous)
      .orElse(recalculatePointsScored(event, previous))
      .orElse {
        if (event.isEmpty) None else Some(event)
      }
  }

  def normalize(event: Event, previousOpt: Option[Event]): Option[Event] =
    accept(event).flatMap(e =>
      previousOpt match {
        case None => normalizeFirst(e)
        case Some(p) =>
          recalculateMatchScore(e, p)
            .orElse(recalculatePointsScored(e, p))
    })

  def recalculateMatchScore(event: Event, previous: Event): Option[Event] =
    if (event.pointsScored > 0) {
      val deltaScore = if (event.whoScored == 0) (event.pointsScored, 0) else (0, event.pointsScored)
      Some(event.copy(currentMatchScore = MatchScore.sum(previous.currentMatchScore, deltaScore)))
    } else None

  def recalculatePointsScored(event: Event, previous: Event): Option[Event] = {
    val deltaScore = MatchScore.delta(event.currentMatchScore, previous.currentMatchScore)
    deltaScore match {
      case (0, 0)                       => None
      case (p1, p2) if p1 > 0 && p2 > 0 => None
      case (p1, p2) =>
        val (whoScored, pointsScored) =
          if (p1 == 0) (1, p2) else (0, p1)
        Some(event.copy(whoScored = whoScored, pointsScored = pointsScored))
    }
  }

  object MatchScore {
    def sum(s1: (Int, Int), s2: (Int, Int)): (Int, Int) = (s1._1 + s2._1, s1._2 + s2._2)
    def delta(s1: (Int, Int), s2: (Int, Int)): (Int, Int) = (Math.max(0, s1._1 - s2._1), Math.max(0, s1._2 - s2._2))
  }
}
