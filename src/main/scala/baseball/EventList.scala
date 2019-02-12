package baseball

import scala.annotation.tailrec

/**
  * General purpose sorted list of events with built-in normalization of subsequent events.
  * Uses simple insertion sort based on assumption that most events will be received in order.
  *
  * @param normalize function normalizing following event
  * @param ordering canonical ordering of events
  * @tparam E event type
  */
class EventList[E](normalize: (E, Option[E]) => Option[E])(implicit ordering: Ordering[E]) {

  @volatile
  private var list = List.empty[E]

  def receive(event: E): this.type = {
    list = insert(Nil, event, list)
    this
  }

  def recent: Option[E] = list.headOption

  def events(limit: Option[Int] = None): List[E] = limit.map(list.take).getOrElse(list)

  @tailrec
  private def insert(later: List[E], event: E, earlier: List[E]): List[E] = earlier match {
    case Nil =>
      rebuild(later, event, None, Nil)
    case head :: tail =>
      if (event == head) list
      else if (ordering.gteq(event, head)) rebuild(later, event, Some(head), tail)
      else insert(head :: later, event, tail)
  }

  @tailrec
  private def rebuild(later: List[E], event: E, previous: Option[E], earlier: List[E]): List[E] =
    normalize(event, previous) match {
      case None =>
        later match {
          case Nil          => previous.map(_ :: earlier).getOrElse(earlier)
          case head :: tail => rebuild(tail, head, previous, earlier)
        }
      case Some(reconciledEvent) =>
        later match {
          case Nil          => reconciledEvent :: previous.map(_ :: earlier).getOrElse(earlier)
          case head :: tail => rebuild(tail, head, Some(reconciledEvent), previous.map(_ :: earlier).getOrElse(earlier))
        }
    }

}
