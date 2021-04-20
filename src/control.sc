object control {
	import scala.util.control.Breaks._

	//Problem 1
	def tax(income: Double) =
     income match {
				case error if income < 0 => throw new Exception("InvalidIncome Exception")
				case income1 if income < 20000 => 0 * income
				case income2 if income < 30000 => .05 * income
				case income3 if income < 40000 => .11 * income
				case income4 if income < 60000 => .23 * income
				case income5 if income < 100000 => .32 * income
				case income6 if income >= 100000 => .5 * income
     }                                            //> tax: (income: Double)Double
     
  tax(99)                                         //> res0: Double = 0.0
  tax(25000)                                      //> res1: Double = 1250.0
  tax(35000)                                      //> res2: Double = 3850.0
  tax(45000)                                      //> res3: Double = 10350.0
  tax(65000)                                      //> res4: Double = 20800.0
  tax(10000000)                                   //> res5: Double = 5000000.0
  try {
  		tax(-100)
  } catch {
  		case e: Exception => println(e)
  }                                               //> java.lang.Exception: InvalidIncome Exception
                                                  //| res6: AnyVal = ()


	//Problem 2
	def drawRectangle(n: Int, m: Int) {
		for (i <- 1 to n) {
			for(j <-1 to m) print("*")
			println("")
		}
	}                                         //> drawRectangle: (n: Int, m: Int)Unit
drawRectangle(2,3)                                //> ***
                                                  //| ***
drawRectangle(4,10)                               //> **********
                                                  //| **********
                                                  //| **********
                                                  //| **********
	                                                  
	//Problem 3
	def printSums(num1: Int, num2: Int) {
		for (i <- 1 to num1 - 1) {
			for(j <-1 to num2 - 1) println(i + " + " + j + " = " + (i+j))
		}
	}                                         //> printSums: (num1: Int, num2: Int)Unit
	printSums(4,3)                            //> 1 + 1 = 2
                                                  //| 1 + 2 = 3
                                                  //| 2 + 1 = 3
                                                  //| 2 + 2 = 4
                                                  //| 3 + 1 = 4
                                                  //| 3 + 2 = 5
	                                                  
	//Problem 4
	class EscapeDemo {
		def mystery() {
			breakable {
				for(i <- 1 to 99) {
					breakable {
						if (i % 3 == 0) break// skip if i divisible by 3
						if (i == 10) break // terminate loop when i is 10
						println("i = " + i)
					}
					if (i == 10) break // terminate loop when i is 10
				}
			}
				println("done")
		}
	}
	
	val instance: EscapeDemo = new EscapeDemo //> instance  : control.EscapeDemo = control$EscapeDemo$1@400cff1a
	instance.mystery()                        //> i = 1
                                                  //| i = 2
                                                  //| i = 4
                                                  //| i = 5
                                                  //| i = 7
                                                  //| i = 8
                                                  //| done
	
	//Problem 5
	def root(x: Double): Option[Double] = if (x < 0) None else Some(Math.sqrt(x))
                                                  //> root: (x: Double)Option[Double]
	def below10(x: Double): Option[Double] = if (x < 10) Some(x) else None
                                                  //> below10: (x: Double)Option[Double]
	
	def pureRoot(x: Option[Double]): Option[Double] =
		x match {
			case Some(y) => root(y)
			case None => None
		}                                 //> pureRoot: (x: Option[Double])Option[Double]
	
	def pureBelow10(x: Option[Double]): Option[Double] =
		x match {
				case Some(y) => below10(y)
				case None => None
		}                                 //> pureBelow10: (x: Option[Double])Option[Double]
		
	def below10root(x: Option[Double]): Option[Double] = pureRoot(pureBelow10(x))
                                                  //> below10root: (x: Option[Double])Option[Double]
	
	pureRoot(Option(49))                      //> res7: Option[Double] = Some(7.0)
	pureRoot(Option(-49))                     //> res8: Option[Double] = None
	pureRoot(None)                            //> res9: Option[Double] = None
	pureBelow10(Option(9))                    //> res10: Option[Double] = Some(9.0)
	pureBelow10(Option(16))                   //> res11: Option[Double] = None
	pureRoot(None)                            //> res12: Option[Double] = None
	below10root(Option(4))                    //> res13: Option[Double] = Some(2.0)
	below10root(Option(16))                   //> res14: Option[Double] = None
	below10root(None)                         //> res15: Option[Double] = None

}