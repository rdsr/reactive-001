package week2;

object SimulationPlay extends Circuits with Parameters {
  val in1, in2, sum, carry = new Wire             //> in1  : week2.SimulationPlay.Wire = week2.Gates$Wire@4c63c68
                                                  //| in2  : week2.SimulationPlay.Wire = week2.Gates$Wire@4e4d6444
                                                  //| sum  : week2.SimulationPlay.Wire = week2.Gates$Wire@3af1d485
                                                  //| carry  : week2.SimulationPlay.Wire = week2.Gates$Wire@5ab785fe

  halfAdder(in1, in2, sum, carry)
  probe("sum", sum)                               //> sum 0 new-value = false
  probe("carry", carry)                           //> carry 0 new-value = false
  
  in1 setSignal true
  run()                                           //> *** Simulation started, time = 0 ***
                                                  //| sum 8 new-value = true
                                                  //| List()
          
  in2 setSignal true
  run()                                           //> *** Simulation started, time = 8 ***
                                                  //| carry 11 new-value = true
                                                  //| sum 16 new-value = false
                                                  //| List()
           
  
                                                   
  in1 setSignal false
  run()                                           //> *** Simulation started, time = 16 ***
                                                  //| carry 19 new-value = false
                                                  //| sum 24 new-value = true
                                                  //| List()
}