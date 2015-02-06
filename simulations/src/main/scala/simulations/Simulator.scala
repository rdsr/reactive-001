package simulations

class Simulator {
  type Action = () ⇒ Unit

  protected type Agenda = List[WorkItem]

  case class WorkItem(time: Int, action: Action)

  protected[simulations] var agenda: Agenda = List()
  protected var currentTime = 0

  def insert(ag: Agenda, item: WorkItem): Agenda =
    if (ag.isEmpty || item.time < ag.head.time) item :: ag
    else ag.head :: insert(ag.tail, item)

  protected def afterDelay(delay: Int)(action: ⇒ Unit) {
    val item = WorkItem(currentTime + delay, () ⇒ action)
    agenda = insert(agenda, item)
  }

  def addToAgenda(delay: Int, action: Action) {
    agenda = insert(agenda, WorkItem(currentTime + 1, () ⇒ action()))
  }

  protected[simulations] def next {
    agenda match {
      case List() ⇒ {}
      case WorkItem(time, action) :: rest ⇒
        agenda = rest
        currentTime = time
        action()
    }
  }

  def run {
    println("*** New propagation ***")
    while (!agenda.isEmpty) { next }
  }
}
