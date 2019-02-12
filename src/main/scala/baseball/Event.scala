package baseball

case class Event(matchTime: Int, currentMatchScore: (Int, Int), whoScored: Int, pointsScored: Int) {

  def isEmpty: Boolean = pointsScored == 0 && currentMatchScore._1 == 0 & currentMatchScore._2 == 0
}

object Event {
  implicit final val ordering: Ordering[Event] =
    Ordering.fromLessThan[Event]((e1, e2) => e1.matchTime <= e2.matchTime)
}
