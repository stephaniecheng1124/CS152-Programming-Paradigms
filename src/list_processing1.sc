object list_processing1 {
	import scala.util.control.Breaks._
	
	//Problem 1 *************************************************************************************************************
	//Write a function that computes the sum of cubes of all odd numbers occurring in a list of integers.
	
	def sumOfCubesIterative(l: List[Int]) = {
		var sum = 0
		for(i <- 0 until l.size) {
   			if (l(i) % 2 == 1) sum += Math.pow(l(i), 3).toInt
  		}
  		sum
	}                                         //> sumOfCubesIterative: (l: List[Int])Int
	
	def sumOfCubesRecursive(l: List[Int]): Int = {
		if (l == Nil) 0
		else {
			if (l.head % 2 == 1) sumOfCubesRecursive(l.tail) + Math.pow(l.head, 3).toInt
			else sumOfCubesRecursive(l.tail) + 0
		}

	}                                         //> sumOfCubesRecursive: (l: List[Int])Int
	
	def sumOfCubesTail(l: List[Int]) = {
		def helper(list: List[Int], result: Int): Int = {
			if (list == Nil) result
			else {
				if (list.head % 2 == 1) helper(list.tail, result + Math.pow(list.head, 3).toInt)
				else helper(list.tail, result)
			}
		}
		helper(l, 0)
	}                                         //> sumOfCubesTail: (l: List[Int])Int
	
	def SumOfCubesMFR(l: List[Int]) = {
		def isOdd(num: Int) = if (num % 2 == 1) true else false
		val oddList = l.filter(isOdd _)
		
		var sum = 0
		for(i <- 0 until oddList.size) sum += Math.pow(oddList(i), 3).toInt
		sum
	}                                         //> SumOfCubesMFR: (l: List[Int])Int
	
	
	val nums = List(1, 2, 3, 4, 5)            //> nums  : List[Int] = List(1, 2, 3, 4, 5)
	sumOfCubesIterative(nums)                 //> res0: Int = 153
	sumOfCubesRecursive(nums)                 //> res1: Int = 153
	sumOfCubesTail(nums)                      //> res2: Int = 153
	SumOfCubesMFR(nums)                       //> res3: Int = 153
	
	
	
	//Problem 2 (4 versions) ***********************************************************************************************
	//Write a function that computes the sum of numbers in a list of lists of numbers:

	//Helper add method
	def recursiveSum(nums: List[Int]): Int = {
    if (nums == Nil) 0 else nums.head + recursiveSum(nums.tail)
	}                                         //> recursiveSum: (nums: List[Int])Int
	
	
	def sumOfSumsIterative(listOfLists: List[List[Int]]) = {
		var sum = 0
		if (listOfLists != Nil) {
			for (i <- listOfLists) {
				for (j <- i) sum += j
			}
		}
		sum
	}                                         //> sumOfSumsIterative: (listOfLists: List[List[Int]])Int
	
	def sumOfSumsRecursive(listOfLists: List[List[Int]]): Int = {
		if (listOfLists == Nil) 0
		else recursiveSum(listOfLists.head) + sumOfSumsRecursive(listOfLists.tail)
	}                                         //> sumOfSumsRecursive: (listOfLists: List[List[Int]])Int
	
  def sumOfSumsTail(listOfLists: List[List[Int]]) = {
  		def helper(l: List[List[Int]], result: Int): Int = {
  			if (l == Nil) result
			else helper(l.tail, result + recursiveSum(l.head))
  		}
		helper(listOfLists, 0)
	}                                         //> sumOfSumsTail: (listOfLists: List[List[Int]])Int
	
	def sumOfSumsMFR(listOfLists: List[List[Int]]) = {
			recursiveSum(listOfLists.map(recursiveSum _))
	}                                         //> sumOfSumsMFR: (listOfLists: List[List[Int]])Int
	
	val list1: List[Int] = List(1,2,3)        //> list1  : List[Int] = List(1, 2, 3)
	val list2: List[Int] = List(4,5,6)        //> list2  : List[Int] = List(4, 5, 6)
	val listOfLists = List(list1, list2)      //> listOfLists  : List[List[Int]] = List(List(1, 2, 3), List(4, 5, 6))
	
	
	sumOfSumsIterative(listOfLists)           //> res4: Int = 21
	sumOfSumsRecursive(listOfLists)           //> res5: Int = 21
	sumOfSumsTail(listOfLists)                //> res6: Int = 21
	sumOfSumsMFR(listOfLists)                 //> res7: Int = 21
	
	
	//Problem 3 *************************************************************************************************************
	//depth(List(List(List 1, 2, List(3)))) = 4
	
	def depth(thing: Any): Int =
	thing match {
      case Nil => 1
      case h::t => depth(h) + depth(t)
      case _ => 0
   }                                              //> depth: (thing: Any)Int
   
   depth(List(List(List (1, 2), List(3, 2))))     //> res8: Int = 4
   depth(List(List(3), 3))                        //> res9: Int = 2

	//Problem 6 (4 versions) ************************************************************************************************
	//Write a function that returns the number of elements in a list that satisfy a given predicate.
	//(The predicate is a parameter of type T=>Boolean.)
	
	def numSatisfiedIterative[T](list: List[T], pred: T=>Boolean) = {
		var count = 0
		for(i <- list) if (pred(i)) count += 1
    count
	}                                         //> numSatisfiedIterative: [T](list: List[T], pred: T => Boolean)Int
	
	
	def numSatisfiedRecursive[T](list: List[T], pred: T=>Boolean): Int = {
		if (list == Nil) 0
		else {
			if (pred(list.head)) 1 + numSatisfiedRecursive(list.tail, pred)
			else numSatisfiedRecursive(list.tail, pred)
		}
	}                                         //> numSatisfiedRecursive: [T](list: List[T], pred: T => Boolean)Int
	
	def numSatisfiedTail[T](list: List[T], pred: T=>Boolean) = {
		def countHelper(result: Int, l: List[T]): Int = {
			if (l == Nil) result
			else if (pred(l.head)) countHelper(result + 1, l.tail)
			else countHelper(result, l.tail )
		}
		countHelper(0, list)
	}                                         //> numSatisfiedTail: [T](list: List[T], pred: T => Boolean)Int
	
	
	def numSatisfiedMFR[T](list: List[T], pred: T=>Boolean) = {
		val passingList = list.filter(pred)
		passingList.size
	}                                         //> numSatisfiedMFR: [T](list: List[T], pred: T => Boolean)Int
	
	
	numSatisfiedIterative(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res10: Int = 2
	numSatisfiedIterative(List("racecar","mom","lol","Hi"), (x: String) => x == x.reverse)
                                                  //> res11: Int = 3
              
  numSatisfiedRecursive(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res12: Int = 2
	numSatisfiedRecursive(List("racecar","mom","lol","Hi"), (x: String) => x == x.reverse)
                                                  //> res13: Int = 3
                                                          
	numSatisfiedTail(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res14: Int = 2
  numSatisfiedTail(List("racecar","mom","lol","Hi"), (x: String) => x == x.reverse)
                                                  //> res15: Int = 3
        
	numSatisfiedMFR(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res16: Int = 2
	numSatisfiedMFR(List("racecar","mom","lol","Hi"), (x: String) => x == x.reverse)
                                                  //> res17: Int = 3
	
	//Problem 7 (4 versions) **********************************************************************************************
	//Write a function that returns true if all elements in a list satisfy a given predicate.
	def allSatisfiedIterative[T](list: List[T], pred: T=>Boolean) = {
		var satisfied = false
		breakable {
			for(i <- list) if (!pred(i)) break
    		satisfied = true
		}
		satisfied
	}                                         //> allSatisfiedIterative: [T](list: List[T], pred: T => Boolean)Boolean
	
	def allSatisfiedRecursive[T](list: List[T], pred: T=>Boolean): Boolean = {
		if (list == Nil) true
		else {
			if (pred(list.head)) allSatisfiedRecursive(list.tail, pred)
			else false
		}
	}                                         //> allSatisfiedRecursive: [T](list: List[T], pred: T => Boolean)Boolean
	
	def allSatisfiedTail[T](list: List[T], pred: T=>Boolean) = {
		def countHelper(l: List[T]): Boolean = {
			if (l == Nil) true
			else if (pred(l.head)) countHelper(l.tail)
			else false
		}
		countHelper(list)
	}                                         //> allSatisfiedTail: [T](list: List[T], pred: T => Boolean)Boolean
	
	def allSatisfiedMFR[T](list: List[T], pred: T=>Boolean) = {
		val passingList = list.filter(pred)
		list.size == passingList.size
	}                                         //> allSatisfiedMFR: [T](list: List[T], pred: T => Boolean)Boolean


	allSatisfiedIterative(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res18: Boolean = false
  allSatisfiedIterative(List(2,4,6,8), (x: Int) => x % 2 == 0)
                                                  //> res19: Boolean = true
	allSatisfiedRecursive(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res20: Boolean = false
	allSatisfiedRecursive(List(2,4,6,8), (x: Int) => x % 2 == 0)
                                                  //> res21: Boolean = true
  allSatisfiedTail(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res22: Boolean = false
  allSatisfiedTail(List(2,4,6,8), (x: Int) => x % 2 == 0)
                                                  //> res23: Boolean = true
  allSatisfiedMFR(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res24: Boolean = false
  allSatisfiedMFR(List(2,4,6,8), (x: Int) => x % 2 == 0)
                                                  //> res25: Boolean = true
	
	//Problem 8 (4 versions) **********************************************************************************************
	//Write a function that returns true if any element in a list satisfies a given predicate.
		def anySatisfiedIterative[T](list: List[T], pred: T=>Boolean) = {
		var satisfied = true
		breakable {
			for(i <- list) if (pred(i)) break
    		satisfied = false
		}
		satisfied
	}                                         //> anySatisfiedIterative: [T](list: List[T], pred: T => Boolean)Boolean
	
	def anySatisfiedRecursive[T](list: List[T], pred: T=>Boolean): Boolean = {
		if (list == Nil) false
		else {
			if (!pred(list.head)) anySatisfiedRecursive(list.tail, pred)
			else true
		}
	}                                         //> anySatisfiedRecursive: [T](list: List[T], pred: T => Boolean)Boolean
	
	def anySatisfiedTail[T](list: List[T], pred: T=>Boolean) = {
		def countHelper(l: List[T]): Boolean = {
			if (l == Nil) false
			else if (!pred(l.head)) countHelper(l.tail)
			else true
		}
		countHelper(list)
	}                                         //> anySatisfiedTail: [T](list: List[T], pred: T => Boolean)Boolean
	
	
	def anySatisfiedMFR[T](list: List[T], pred: T=>Boolean) = {
		val passingList = list.filter(pred)
		passingList.size != 0
	}                                         //> anySatisfiedMFR: [T](list: List[T], pred: T => Boolean)Boolean


	anySatisfiedIterative(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res26: Boolean = true
  anySatisfiedIterative(List(1,3,5,7), (x: Int) => x % 2 == 0)
                                                  //> res27: Boolean = false
	anySatisfiedRecursive(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res28: Boolean = true
	anySatisfiedRecursive(List(1,3,5,7), (x: Int) => x % 2 == 0)
                                                  //> res29: Boolean = false
  anySatisfiedTail(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res30: Boolean = true
  anySatisfiedTail(List(1,3,5,7), (x: Int) => x % 2 == 0)
                                                  //> res31: Boolean = false
  anySatisfiedMFR(List(2,3,4,5), (x: Int) => x % 2 == 0)
                                                  //> res32: Boolean = true
  anySatisfiedMFR(List(1,3,5,7), (x: Int) => x % 2 == 0)
                                                  //> res33: Boolean = false

	//Problem 10 ************************************************************************************************************
	//Write a function that returns true if a given list of integers is sorted (in ascending order).
	def sorted(list: List[Int]) = {
		var sorted = false
		breakable {
			if (list.size > 1) {
				for(i <- 1 to list.size - 1) if (list(i) < list(i -1)) break
			}
    		sorted = true
		}
		sorted
	}                                         //> sorted: (list: List[Int])Boolean
	
	sorted(List(1,2,3,4))                     //> res34: Boolean = true
	sorted(List(5,1,2,3))                     //> res35: Boolean = false
	
	//Problem 13 ************************************************************************************************************
	
	//An infinitely long stream of 1's
	def makeOnes(n: Int): Stream[Int] = n #:: makeOnes(1)
                                                  //> makeOnes: (n: Int)Stream[Int]
  val ones = makeOnes(1)                          //> ones  : Stream[Int] = Stream(1, ?)
  ones.head                                       //> res36: Int = 1
  ones.tail                                       //> res37: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  ones.tail.tail                                  //> res38: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  ones.tail.tail.tail                             //> res39: scala.collection.immutable.Stream[Int] = Stream(1, ?)

	//The stream of all non-negative integers
	def makeNonNeg(n: Int): Stream[Int] = n #:: makeNonNeg(n + 1)
                                                  //> makeNonNeg: (n: Int)Stream[Int]
  val nonNeg = makeNonNeg(0)                      //> nonNeg  : Stream[Int] = Stream(0, ?)
	nonNeg.head                               //> res40: Int = 0
  nonNeg.tail                                     //> res41: scala.collection.immutable.Stream[Int] = Stream(1, ?)
  nonNeg.tail.tail                                //> res42: scala.collection.immutable.Stream[Int] = Stream(2, ?)
  nonNeg.tail.tail.tail                           //> res43: scala.collection.immutable.Stream[Int] = Stream(3, ?)
  nonNeg.tail.tail.tail.tail                      //> res44: scala.collection.immutable.Stream[Int] = Stream(4, ?)
	
	//The stream of all non-negative even integers
	def makeNonNegEven(n: Int): Stream[Int] = n #:: makeNonNegEven(n + 2)
                                                  //> makeNonNegEven: (n: Int)Stream[Int]
	val nonNegEven = makeNonNegEven(0)        //> nonNegEven  : Stream[Int] = Stream(0, ?)
	nonNegEven.head                           //> res45: Int = 0
  nonNegEven.tail                                 //> res46: scala.collection.immutable.Stream[Int] = Stream(2, ?)
  nonNegEven.tail.tail                            //> res47: scala.collection.immutable.Stream[Int] = Stream(4, ?)
  nonNegEven.tail.tail.tail                       //> res48: scala.collection.immutable.Stream[Int] = Stream(6, ?)
  nonNegEven.tail.tail.tail.tail                  //> res49: scala.collection.immutable.Stream[Int] = Stream(8, ?)
  
	//The stream of all squares of integers
	def makeSquares(n: Int): Stream[Int] = n #:: makeSquares(Math.pow(n + 1, 2).toInt)
                                                  //> makeSquares: (n: Int)Stream[Int]
	val squares = makeSquares(0)              //> squares  : Stream[Int] = Stream(0, ?)
	squares.head                              //> res50: Int = 0
	squares.tail                              //> res51: scala.collection.immutable.Stream[Int] = Stream(1, ?)
	squares.tail.tail                         //> res52: scala.collection.immutable.Stream[Int] = Stream(4, ?)
	squares.tail.tail.tail                    //> res53: scala.collection.immutable.Stream[Int] = Stream(25, ?)
	squares.tail.tail.tail.tail               //> res54: scala.collection.immutable.Stream[Int] = Stream(676, ?)
 
}