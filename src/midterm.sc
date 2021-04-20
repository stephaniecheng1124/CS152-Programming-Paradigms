object midterm {

	//Problem 1
	class Entry(val temp: Double, val mon: Int, val day: Int)

  //average temperature in degrees Fahrenheit for all readings taken in month
  //map, reduce, filter
  
	
	def avgTemp(month: Int, log: List[Entry]): Double = {
		
		def avg(nums: List[Entry]): Double = {
	    if (nums.length == 0) throw new Exception("length = 0")
		    var sum = 0.0
		    for(i <- nums) sum += i.temp
		    sum / nums.length
	    }
		
		def mapDelete(e: Entry) = if (e.mon != month) "" else e.mon
		def monthFilter(e: Entry) = if (e.mon == "") false else true
		
		avg(log.map(mapDelete _).filter(monthFilter _))
	
	}
	val log = List(new Entry(20, 1, 1), new Entry(25, 1, 2), new Entry(15, 2, 2), new Entry(10, 1, 3), new Entry(22, 6, 5))
	avgTemp(1, log) // = 39.0

	
	
	
	
	
	
	//Problem 2
	class Knight(val name: String) {
		var strategy: AttackStrategy
		private var health: Int = 100
		
		def damage(amount: Int) {
			if (health - amount < 0) {
				health = 0
			}
			else health -= amount
		}
		
		def getHealth() = health

		def attack(victim: Knight) {
			println(name + " is attacking " + victim.name)
			strategy.attack(victim)
		}
		
	}
	
	trait AttackStrategy {
	  def attack(opponent: Knight) {
			println("Attacking" + opponent.name)
			opponent.damage(10)
	  }
	}
	
	object Stab extends AttackStrategy {
		override def attack(opponent: Knight) {
			if (opponent.getHealth > 0) {
				println("stabbing")
				opponent.damage(10)
				println(opponent.name + ".health = " + opponent.getHealth)
				if(opponent.getHealth == 0) println(opponent.name + " is dead!!!")
			}
			else println("You are already dead!")
		}
	}
	
	object Mace extends AttackStrategy {
		override def attack(opponent: Knight) {
			if (opponent.getHealth > 0) {
				println("macing")
				opponent.damage(10)
				println(opponent.name + ".health = " + opponent.getHealth)
				if(opponent.getHealth == 0) println(opponent.name + " is dead!!!")
			}
			else println("You are already dead!")
		}
	}
	
	class CompositeStrategy (strat: List[AttackStrategy]) extends AttackStrategy {
		override def attack(opponent: Knight) {
			if (opponent.getHealth > 0) {
				
				for(s <- strat) {
					if (s == Stab) {
						println("stabbing")
						opponent.damage(10)
					}
					else {
						println("macing")
						opponent.damage(10)
					}
					
				println(opponent.name + ".health = " + opponent.getHealth)
	
			}
				if(opponent.getHealth == 0) println(opponent.name + " is dead!!!")
			
			}
			else println("You are already dead!")
		}
	}
	
	val k1 = new Knight("Drobot")
	val k2 = new Knight("Baldimore")
	k1.strategy = Mace
	k2.strategy = new CompositeStrategy(List(Mace, Stab, Stab))
	k1.attack(k2)
	k2.attack(k1)




	//Problem 3
	//Combinator takes a function and output a function of the other type
	
	
	trait Value
	class Number(val value: Int) extends Value {
	   override def toString = "Number(" + value + ")"
	}
	class Boole(val value: Boolean) extends Value {
	   override def toString = "Boole(" + value + ")"
	}
		
	def compose(f: Int=>Int): Value=>Option[Number] = {
	
   def r(v: Value): Option[Number] = {
   	try { 
   		some(f(v))
   	} catch {
   		case e: Exception => None
   	}
   
   }
   
   r _
	}    
	
	
	
	
	
	
	
	
	
	
	
	
	
	

}