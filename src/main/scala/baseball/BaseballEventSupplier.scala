package baseball

object BaseballEventSupplier {

  def decode(event: Int): Event = {
    val pointsScored = event & 0x3
    val whoScored = event >> 2 & 0x1
    val totalPointsTeam2 = event >> 3 & 0xFF
    val totalPointsTeam1 = event >> 11 & 0xFF
    val matchTime = event >> 19 & 0xFFF
    Event(matchTime, (totalPointsTeam1, totalPointsTeam2), whoScored, pointsScored)
  }

  def encode(event: Event): Int =
    event.matchTime << 19 | (event.currentMatchScore._1 << 11) | (event.currentMatchScore._2 << 3) | (event.whoScored << 2) | event.pointsScored

}
