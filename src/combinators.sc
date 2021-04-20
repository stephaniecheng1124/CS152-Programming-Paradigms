object combinators {

	//Problem 1
	def compose[A,B,C](f: B=>C, g: A=>B): A=>C = {
   def r(x: A): C = f(g(x));
   r _
	}                                         //> compose: [A, B, C](f: B => C, g: A => B)A => C
	def triple(x: Int): Int = 3 * x           //> triple: (x: Int)Int
	def square(x: Int): Int = x * x           //> square: (x: Int)Int
	
	val squareTriple = compose(square _, triple)
                                                  //> squareTriple  : Int => Int = combinators$$$Lambda$12/592959754@36f0f1be
	squareTriple(2)                           //> res0: Int = 36
	squareTriple(3)                           //> res1: Int = 81


	//Problem 2
	def selfIter[T](f: T=>T, n: Int): T=>T = if ( n == 0) f else compose(f, selfIter(f , n - 1))
                                                  //> selfIter: [T](f: T => T, n: Int)T => T
	def inc(x: Double) = x + 1                //> inc: (x: Double)Double
	def doublez(x: Double) = 2 * x            //> doublez: (x: Double)Double

	val composedInc = selfIter(inc _, 2)      //> composedInc  : Double => Double = combinators$$$Lambda$12/592959754@525f1e4e
                                                  //| 
	val composedDouble = selfIter(doublez _, 2)
                                                  //> composedDouble  : Double => Double = combinators$$$Lambda$12/592959754@52aa2
                                                  //| 946
	composedInc(0)                            //> res2: Double = 3.0
	composedInc(2)                            //> res3: Double = 5.0
	composedDouble(2)                         //> res4: Double = 16.0
	
	//Problem 3  //make it recursive??
	def countPass[T](arr: Array[T], f: T=>Boolean) = {
	
		def countHelper(count: Int, result: Int, a: Array[T]): Int = {
			if (count >= a.size) result
			else if (f(a(count))) countHelper(count + 1, result + 1, a)
			else countHelper(count + 1, result, a)
		}
		countHelper(0, 0, arr)
	}                                         //> countPass: [T](arr: Array[T], f: T => Boolean)Int
	
	countPass(Array(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res5: Int = 2
  countPass(Array("racecar","mom","lol","Hi"), (x: String) => x == x.reverse)
                                                  //> res6: Int = 3
	

	//Problem 4
	def recur(baseVal: Int, combiner: (Int, Int)=>Int): Int=>Int = {
			def recursive(x: Int): Int = if (x == 0) baseVal else combiner(x , recursive(x - 1))
			recursive _
	}                                         //> recur: (baseVal: Int, combiner: (Int, Int) => Int)Int => Int
	
	//Use the recur combinator to implement a factorial function:
	def multiplier(a: Int, b: Int) = a * b    //> multiplier: (a: Int, b: Int)Int
	val factorial = recur(1, multiplier _)    //> factorial  : Int => Int = combinators$$$Lambda$18/1763344271@2d9d4f9d
	factorial(3)                              //> res7: Int = 6
	factorial(4)                              //> res8: Int = 24
	
	//Problem 5
	//unary: an operation consisting of only one element
	def deOptionize[A, T](optionFunc: A=>Option[T]) = {
		def exceptionFunc(digits: A) = {
			optionFunc(digits) match {
				case Some(y) => y
				case None => throw new Exception("Invalid input!!")
			}
		}
		exceptionFunc _
	}                                         //> deOptionize: [A, T](optionFunc: A => Option[T])A => T
	
	def parseDigits(digits: String): Option[Int] = if (digits.matches("[0-9]*")) Some(digits.toInt) else None
                                                  //> parseDigits: (digits: String)Option[Int]
	val deOptioned = deOptionize(parseDigits) //> deOptioned  : String => Int = combinators$$$Lambda$20/240166646@14ec4505

	deOptioned("2323")                        //> res9: Int = 2323
	deOptioned("12")                          //> res10: Int = 12
	try {
		deOptioned("AA12")
	} catch {
		case e : Exception => println(e)
	}                                         //> java.lang.Exception: Invalid input!!
                                                  //| res11: AnyVal = ()


}