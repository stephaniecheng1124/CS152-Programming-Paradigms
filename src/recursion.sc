object recursion {
   
	def dec(n: Int) = n - 1                   //> dec: (n: Int)Int
  def inc(n: Int) = n + 1                         //> inc: (n: Int)Int
	def isZero(n: Int) = n == 0               //> isZero: (n: Int)Boolean
  
  
  //Problem 1
  def add(n: Int, m: Int): Int = if (isZero(m)) n else inc(add(n, dec(m)))
                                                  //> add: (n: Int, m: Int)Int
  
  add(0,1)                                        //> res0: Int = 1
  add(2,0)                                        //> res1: Int = 2
  add(3,4)                                        //> res2: Int = 7
  add(2,3)                                        //> res3: Int = 5
  
  //Problem 2
  def mul(n: Int, m: Int): Int = if (isZero(m) || isZero(n)) 0 else add(n, mul(n, dec(m)))
                                                  //> mul: (n: Int, m: Int)Int
  
  mul(2,3)                                        //> res4: Int = 6
  mul(5,2)                                        //> res5: Int = 10
  mul(4,1)                                        //> res6: Int = 4
  mul(9,0)                                        //> res7: Int = 0
  mul(0,3)                                        //> res8: Int = 0
  
  //Problem 3
  def exp2(m: Int): Int = if (isZero(m)) 1 else mul(2, exp2(dec(m)) )
                                                  //> exp2: (m: Int)Int
  
  exp2(0)                                         //> res9: Int = 1
  exp2(1)                                         //> res10: Int = 2
  exp2(2)                                         //> res11: Int = 4
  exp2(3)                                         //> res12: Int = 8
  
  
  //Problem 4
  def hyperExp(n: Int): Int = if (isZero(n)) exp2(0) else exp2(hyperExp(dec(n))) // n-times
                                                  //> hyperExp: (n: Int)Int
  hyperExp(0)                                     //> res13: Int = 1
  hyperExp(1)                                     //> res14: Int = 2
  hyperExp(2)                                     //> res15: Int = 4
  hyperExp(3)                                     //> res16: Int = 16
  //hyperExp(4) gives a stack overflow
  
  
  
  //Problem 5 - remember to convert minus to dec!!
  
  //Yes, using tail recursion improves the stack overflow problem because it allows reuse of the stack.
  //The space complexity is O(1), however the time complexity is still O(n)
  
  def tailAdd(n: Int, m: Int) = {
  		def helper(count: Int, result: Int): Int =
      if (isZero(count)) result else helper(dec(count), inc(result))
   helper(m, n)
	}                                         //> tailAdd: (n: Int, m: Int)Int
  tailAdd(1,3)                                    //> res17: Int = 4
  tailAdd(0,2)                                    //> res18: Int = 2
  tailAdd(4,0)                                    //> res19: Int = 4
  tailAdd(4,3)                                    //> res20: Int = 7
  
   def tailMul(n: Int, m: Int) = {
   	def helper(count: Int, result: Int): Int =
   		if (isZero(count)) result else helper(dec(count), tailAdd(result, n))
   	helper(m, 0)
   }                                              //> tailMul: (n: Int, m: Int)Int
   tailMul(1,3)                                   //> res21: Int = 3
   tailMul(2,0)                                   //> res22: Int = 0
   tailMul(0,2)                                   //> res23: Int = 0
   
   def tailExp2(m: Int) = {
   	def helper(count: Int, result: Int): Int =
   		if (isZero(count)) result else helper(dec(count), tailMul(result, 2))
   	helper(m, 1)
   }                                              //> tailExp2: (m: Int)Int
   tailExp2(0)                                    //> res24: Int = 1
   tailExp2(1)                                    //> res25: Int = 2
   tailExp2(2)                                    //> res26: Int = 4
   tailExp2(3)                                    //> res27: Int = 8
   tailExp2(4)                                    //> res28: Int = 16
   
	 def tailHyperExp(n: Int) = {
   	def helper(count: Int, result: Int): Int =
   		if (isZero(count)) result else helper(dec(count), tailExp2(result))
   	helper(n, tailExp2(0))
   }                                              //> tailHyperExp: (n: Int)Int
   
   tailHyperExp(0)                                //> res29: Int = 1
   tailHyperExp(1)                                //> res30: Int = 2
   tailHyperExp(2)                                //> res31: Int = 4
   tailHyperExp(3)                                //> res32: Int = 16
   tailHyperExp(4)                                //> res33: Int = 65536

  //Problem 9
  
  //recursive Fibbonacci, get th nth Fibbonacci number
  def fib(n: Int): Int = {
  		if(isZero(n)) 0
  		else if(n == 1) 1
  		else fib(n-2) + fib(n-1)
  }                                               //> fib: (n: Int)Int
  
  fib(0)                                          //> res34: Int = 0
  fib(1)                                          //> res35: Int = 1
  fib(2)                                          //> res36: Int = 1
  fib(3)                                          //> res37: Int = 2
  fib(4)                                          //> res38: Int = 3
  fib(5)                                          //> res39: Int = 5
  fib(6)                                          //> res40: Int = 8
  
  //tail recursive Fibbonacci
   def tailFib(n: Int) = {
   	def helper(result: Int, adder: Int, count: Int): Int =
   		if (isZero(count)) result else helper(result + adder, result, dec(count))
   	helper(0, 1, n)
   }                                              //> tailFib: (n: Int)Int
	tailFib(0)                                //> res41: Int = 0
	tailFib(1)                                //> res42: Int = 1
	tailFib(2)                                //> res43: Int = 1
	tailFib(3)                                //> res44: Int = 2
	tailFib(4)                                //> res45: Int = 3
	tailFib(5)                                //> res46: Int = 5
	tailFib(6)                                //> res47: Int = 8
  
  //Problem 10
  def choose(n: Int, m: Int): Int = {
  		if(isZero(m)) 1
  		else if(m == n) 1
  		else if(m > n) 0
  		else choose(n-1, m-1) + choose(n-1, m)
  }                                               //> choose: (n: Int, m: Int)Int
  choose(5,0)                                     //> res48: Int = 1
  choose(5,1)                                     //> res49: Int = 5
  choose(5,2)                                     //> res50: Int = 10
  choose(5,3)                                     //> res51: Int = 10
  choose(5,4)                                     //> res52: Int = 5
  choose(5,5)                                     //> res53: Int = 1
  choose(5,6)                                     //> res54: Int = 0

}