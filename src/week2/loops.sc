object loops {
	def repeat (command: => Unit)(condition: => Boolean): Unit = {
		command
		if (condition) repeat(command)(condition)
		else ()
	}                                         //> repeat: (command: => Unit)(condition: => Boolean)Unit
	
	for (i <- 0 until 10) {
		println("Hello " + i)             //> Hello 0
                                                  //| Hello 1
                                                  //| Hello 2
                                                  //| Hello 3
                                                  //| Hello 4
                                                  //| Hello 5
                                                  //| Hello 6
                                                  //| Hello 7
                                                  //| Hello 8
                                                  //| Hello 9
	}
	
	(1 to 3) foreach (i =>
	 	"abc" foreach (c => println (i, c)))
                                                  //> (1,a)
                                                  //| (1,b)
                                                  //| (1,c)
                                                  //| (2,a)
                                                  //| (2,b)
                                                  //| (2,c)
                                                  //| (3,a)
                                                  //| (3,b)
                                                  //| (3,c)
	 	
}