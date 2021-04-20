object Accumulator {
 
 	//Problem 6: Accumulator
	class Accumulator() {
		var HALT = false
		var IP = 0
		var program: List[Instruction] = List()
		var register = 0
		
		def run() {
			for(i <- program if HALT == false) i match {
				case i: Halt => HALT = true
				case i: Goto => IP = i.execute(IP)
				case _ => register = i.execute(register)
			}
		}
		
	}
	
	object Accumulator {
		def apply() = new Accumulator()
	}
	
	trait Instruction {
		def execute(reg: Int): Int
	}
	
	class Add(val num: Int) extends Instruction {
	  def execute(reg: Int) = num + reg
	}
	object Add {
	  def apply(operand1: Int) =
	      new Add(operand1)
	}
	
	class Mul(val num: Int) extends Instruction {
	  def execute(register: Int) = num * register
	}
	object Mul {
	  def apply(operand1: Int) =
	      new Mul(operand1)
	}
	
	class Halt() extends Instruction {
	  def execute(register: Int) = register
	}
	object Halt {
	  def apply() = new Halt()
	}
	
	class Goto(ip: Int) extends Instruction {
	  def execute(IP: Int) = ip
	}
	object Goto {
	  def apply(ip: Int) = new Goto(ip)
	}
		
		
	val a = new Accumulator                   //> a  : Accumulator.Accumulator = Accumulator$Accumulator@3043fe0e
		// computing ((3 * 5) + 1) * 2
	a.program = List(Add(3), Mul(5), Add(1), Mul(2))
	a.run()
	a.register                                //> res0: Int = 32
	
	// computing (((10 * 2) + 3) * 5)
	a.register = 0
	a.program = List(Add(10), Mul(2), Add(3), Mul(5))
	a.run()
	a.register                                //> res1: Int = 115
	
	a.register = 0
	a.program = List(Add(10), Mul(2), Add(3), Halt(), Mul(5))
	a.run()
	a.register                                //> res2: Int = 23

	a.register = 0
	a.program = List(Add(10), Mul(2), Add(3), Goto(5), Mul(5), Mul(0))
	a.run()
	a.register                                //> res3: Int = 0
	











}