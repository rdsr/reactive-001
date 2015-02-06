package week2

abstract class Gates extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()

    def getSignal = sigVal

    def setSignal(s: Boolean) =
      if (s != sigVal) {
        sigVal = s
        actions foreach (_())
      }

    def addAction(a: Action) = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val s = input.getSignal
      afterDelay(InverterDelay) {
        output setSignal !s
      }
    }
    input addAction invertAction
  }

  def andGate(a: Wire, b: Wire, o: Wire): Unit = {
    def action(): Unit = {
      val r = a.getSignal & b.getSignal
      afterDelay(AndGateDelay) {
        o setSignal r
      }
    }
    a addAction action
    b addAction action
  }

  def orGate(a: Wire, b: Wire, o: Wire): Unit = {
    def action(): Unit = {
      val r = a.getSignal | b.getSignal
      afterDelay(OrGateDelay) {
        o setSignal r
      }
    }
    a addAction action
    b addAction action
  }

  def probe(name: String, wire: Wire): Unit = {
    def action(): Unit = {
      println(name + " " + currentTime + " new-value = " + wire.getSignal)
    }
    wire addAction action
  }
}