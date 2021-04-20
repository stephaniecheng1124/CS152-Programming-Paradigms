object math {
	import scala.math._
	import scala.util.control.Breaks._
	import scala.util.Random
		  
  //Problem 1
  def solve(a: Double, b: Double, c: Double): Option[(Double, Double)] = {
  		if (Math.pow(b, 2) - (4*a*c) < 0) None
  		else {
  			val root1 = (b + (Math.pow(b, 2) - (4*a*c))) / a*c
  			val root2 = (b - (Math.pow(b, 2) - (4*a*c))) / a*c
  			Some((root1, root2))
  		}
  }                                               //> solve: (a: Double, b: Double, c: Double)Option[(Double, Double)]
  
  solve(1, 0, -1)                                 //> res0: Option[(Double, Double)] = Some((-4.0,4.0))
  solve(2, -2, -4)                                //> res1: Option[(Double, Double)] = Some((-68.0,76.0))
  solve(1, 0, 1)                                  //> res2: Option[(Double, Double)] = None
  
  //0 roots: (b^2 - 4ac) < 0
  //1 root: (b^2 - 4ac) = 0
  //2 roots: (b^2 - 4ac) != 0
  
  
  //Problem 2
  def dist(p1: (Int, Int), p2: (Int, Int)) = {
  		val (x1, y1) = p1
  		val (x2, y2) = p2
  		
  		Math.sqrt(Math.pow((x2 - x1), 2) + Math.pow((y2 - y1), 2))
  }                                               //> dist: (p1: (Int, Int), p2: (Int, Int))Double
  
  dist((1, 1), (0, 0))                            //> res3: Double = 1.4142135623730951
  dist((3, 0), (0, 0))                            //> res4: Double = 3.0
  
  //Type for distance: (p1: (Int, Int), p2: (Int, Int))Double
  //This means that the 2 parameters of dist are tuples containing 2 Ints
  //The output of dist is a Double
  
  
  //Problem 3
  def dot(v1: (Double, Double, Double), v2: (Double, Double, Double)) = {
  		val	(a1, a2, a3) = v1
  		val (b1, b2, b3) = v2
  		
  		a1*b1 + a2*b2 + a3*b3
  }                                               //> dot: (v1: (Double, Double, Double), v2: (Double, Double, Double))Double
  
	dot((2.0, 3, 4), (2, 2.0, 2))             //> res5: Double = 18.0
  
  
  //Problem 6
    def isPrime(x: Int): Boolean =  {
	  		if (x < 0) throw new Exception("Invalid Input: Negative Number")
	  		for (n <- 2 to x) {
	  			if (x % n == 0) return false
	  		}
	  		true
  }                                               //> isPrime: (x: Int)Boolean
  
	isPrime(0)                                //> res6: Boolean = true
	isPrime(1)                                //> res7: Boolean = true
	isPrime(2)                                //> res8: Boolean = false
	isPrime(8)                                //> res9: Boolean = false
  
  
  //Problem 7
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
                                                  //> gcd: (a: Int, b: Int)Int
  
  def phi(n: Int) = {
  		var num = 0
  		for (k <- 1 to n) {
  			if (gcd(n, k) == 1) num += 1
  		}
  		num
  }                                               //> phi: (n: Int)Int
  
  phi(9)                                          //> res10: Int = 6
  phi(10)                                         //> res11: Int = 4
  
  
  //Problem 8
  def rollDice() = {
  
  		val gen = scala.util.Random
  		val a = gen.nextInt(6) + 1
  		val b = gen.nextInt(6) + 1
  		
  		(a,b)
  }                                               //> rollDice: ()(Int, Int)
	
	rollDice                                  //> res12: (Int, Int) = (4,5)
	rollDice                                  //> res13: (Int, Int) = (1,4)
	rollDice                                  //> res14: (Int, Int) = (1,3)
	rollDice                                  //> res15: (Int, Int) = (4,4)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}