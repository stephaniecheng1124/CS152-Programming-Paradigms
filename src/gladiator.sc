object gladiator{

	//Problem 0: Gladiator Lab
	
	trait Crusher {
	  def crush(victim: Gladiator) {
			println("Crush, crush, crushing " + victim.name)
			victim.damage(10)
	  }
	}
	
	trait Masher {
	  def mash(victim: Gladiator) {
			println("Mash, mash, mashing " + victim.name)
			victim.damage(10)
	  }
	}
	
	trait Slasher {
	  def slash(victim: Gladiator) {
			println("Slash, slash, Slashing " + victim.name)
			victim.damage(10)
	  }
	}

	class Gladiator(val name: String) {
		private var health: Int = 100
		
		def damage(amount: Int) {
			if (health - amount < 0) {
				health = 0
			}
			else health -= amount
		
		}
		
		def getHealth() = health

		def attack(victim: Gladiator) {
			println(name + " is attacking " + victim.name)
			victim.damage(5)
		}
		
	}
		
	class CrushMasher(name: String) extends Gladiator(name) with Crusher with Masher {
		private var health = 100
		
		override def attack(opponent: Gladiator) {
			if (health > 0) {
				println(name + " is attacking " + opponent.name)
				super.crush(opponent)
				super.mash(opponent)
				println(opponent.name + "'s health = " + opponent.getHealth)
				if(opponent.getHealth == 0) println(opponent.name + " is dead!!!")
			}
			else println("You are already dead!")
		}
	}
	
	
	val maximus = new CrushMasher("Maximus Prime")
                                                  //> maximus  : gladiator.CrushMasher = gladiator$CrushMasher@335eadca
	
	//singleton
	object bee extends Gladiator("Bumblebee") with Slasher with Masher {
		override def attack(opponent: Gladiator) {
			if (super.getHealth > 0) {
				println(name + " is attacking " + opponent.name)
				super.slash(opponent)
				super.mash(opponent)
				println(opponent.name + "'s health = " + opponent.getHealth)
				if(opponent.getHealth == 0) println(opponent.name + " is dead!!!")
			}
			else println("You are already dead!")
		}
	}
	
	for(i <- 0 to 2) {
     maximus.attack(bee)
     bee.attack(maximus)
	}                                         //> Maximus Prime is attacking Bumblebee
                                                  //| Crush, crush, crushing Bumblebee
                                                  //| Mash, mash, mashing Bumblebee
                                                  //| Bumblebee's health = 80
                                                  //| Bumblebee is attacking Maximus Prime
                                                  //| Slash, slash, Slashing Maximus Prime
                                                  //| Mash, mash, mashing Maximus Prime
                                                  //| Maximus Prime's health = 80
                                                  //| Maximus Prime is attacking Bumblebee
                                                  //| Crush, crush, crushing Bumblebee
                                                  //| Mash, mash, mashing Bumblebee
                                                  //| Bumblebee's health = 60
                                                  //| Bumblebee is attacking Maximus Prime
                                                  //| Slash, slash, Slashing Maximus Prime
                                                  //| Mash, mash, mashing Maximus Prime
                                                  //| Maximus Prime's health = 60
                                                  //| Maximus Prime is attacking Bumblebee
                                                  //| Crush, crush, crushing Bumblebee
                                                  //| Mash, mash, mashing Bumblebee
                                                  //| Bumblebee's health = 40
                                                  //| Bumblebee is attacking Maximus Prime
                                                  //| Slash, slash, Slashing Maximus Prime
                                                  //| Mash, mash, mashing Maximus Prime
                                                  //| Maximus Prime's health = 40
	


}