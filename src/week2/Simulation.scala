package week2

abstract class Simulation {
  type Action = () ⇒ Unit

  case class Event(time: Int, action: Action)

  private var curTime = 0
  def currentTime: Int = curTime

  private var agenda: List[Event] = List()

  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case e :: rest if (e.time <= item.time) ⇒ e :: insert(rest, item)
    case _                                  ⇒ item :: ag
  }

  def afterDelay(delay: Int)(block: ⇒ Unit): Unit = {
    val e = Event(currentTime + delay, () ⇒ block)
    agenda = insert(agenda, e)
  }

  def run() {
    afterDelay(0) {
      println("*** Simulation started, time = " + currentTime + " ***")
    }
    loop()
  }

  private def loop(): Unit = agenda match {
    case first :: rest ⇒
      agenda = rest
      curTime = first.time
      first.action()
      loop()
    case Nil ⇒
  }
}