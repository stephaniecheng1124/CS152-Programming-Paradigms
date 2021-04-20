object list_processing2 {

	import scala.collection.mutable.ListBuffer
	
	//Problem 1 ************************************************************************************************************
  var cs152 = List(List(93.0, 89.0, 90.0), List(45.0, 20.0, 30.0), List(88.0, 82.0, 78.0), List(75.0, 76.0, 68.0))
                                                  //> cs152  : List[List[Double]] = List(List(93.0, 89.0, 90.0), List(45.0, 20.0, 
                                                  //| 30.0), List(88.0, 82.0, 78.0), List(75.0, 76.0, 68.0))

	def avg(scores: List[Double]): Double = {
		if (scores.length == 0) throw new Exception("length is 0")
    var sum = 0.0
    for(i <- scores) sum += i
    sum / scores.length
	}                                         //> avg: (scores: List[Double])Double

	def avgAvg(scores: List[List[Double]]): List[Double] = {
		scores.map(avg _)
	}                                         //> avgAvg: (scores: List[List[Double]])List[Double]

	avgAvg(cs152)                             //> res0: List[Double] = List(90.66666666666667, 31.666666666666668, 82.66666666
                                                  //| 666667, 73.0)

	def passing(scores: List[List[Double]]): List[Int] = {
		val averages = avgAvg(scores)
		def seventyAndUp(num: Double) = if (num >= 70) 1 else 0
		val passOrNot = averages.map(seventyAndUp _)
		var answer = new ListBuffer[Int]()
		for (i <- 0 to passOrNot.size - 1) if (passOrNot(i) == 1) answer += i
		answer.toList
	}                                         //> passing: (scores: List[List[Double]])List[Int]

	passing(cs152)                            //> res1: List[Int] = List(0, 2, 3)
	

	def iterSum(nums: List[Double]): Double = {
    			var result = 0.0
   			for(i <- nums) result += i
   			result
	}                                         //> iterSum: (nums: List[Double])Double
	
	def sumSums(scores: List[List[Double]]): Double = {
			
		iterSum(scores.map(iterSum _))
	}                                         //> sumSums: (scores: List[List[Double]])Double
	sumSums(cs152)                            //> res2: Double = 834.0

	//Problem 2 ************************************************************************************************************
	
		def spellCheck(doc: List[String], dictionary: List[String]): List[String] = {
		var wrongWords = new ListBuffer[String]()
		
		def member(word: String, dict: List[String]): Boolean = {
    if (dict == Nil) false
    else if (word == dict.head) true
    else member(word, dict.tail)
		}
		
		if (doc != Nil) {
			for (i <- doc) if (!member(i, dictionary)) wrongWords += i
		}
		wrongWords.toList
	}                                         //> spellCheck: (doc: List[String], dictionary: List[String])List[String]
	spellCheck(List("hi", "hey", "hello"), List("hi", "hello"))
                                                  //> res3: List[String] = List(hey)

	//Problem 3 ************************************************************************************************************
	
	//map: Apply something to every element in a list
	//filter: Get rid of elements with certain traits
	//reduce: Use comparison to get rid of stuff in the same list
	
	def spellCheckMFR(doc: List[String], dictionary: List[String]): List[String] = {
		def deleteContained(word: String) = if (dictionary.contains(word)) "" else word
		def notEmptyWord(word: String) = if (word.length < 1) false else true
		doc.map(deleteContained _).filter(notEmptyWord _)
	}                                         //> spellCheckMFR: (doc: List[String], dictionary: List[String])List[String]
	
	spellCheckMFR(List("hi", "hey", "hello"), List("hi", "hello"))
                                                  //> res4: List[String] = List(hey)
	
	//Problem 4 ************************************************************************************************************
	def evalMono(mono: (Double, Double), x: Double): Double = mono._1 * Math.pow(x, mono._2)
                                                  //> evalMono: (mono: (Double, Double), x: Double)Double
	
	evalMono((3.0, 2.0), 2.0)                 //> res5: Double = 12.0
	evalMono((-6.0, 0.0), 2.0)                //> res6: Double = -6.0
	evalMono((4.0, 2.0), 2.0)                 //> res7: Double = 16.0
	
	def evalPoly(poly: List[(Double, Double)], x: Double): Double = {
		iterSum(poly.map(evalMono(_, x)))
	}                                         //> evalPoly: (poly: List[(Double, Double)], x: Double)Double
	
	evalPoly(List((3.0, 2.0), (-5.0, 0.0)), 1)//> res8: Double = -2.0
	evalPoly(List((4.0, 2.0), (-6.0, 0.0)), 2)//> res9: Double = 10.0

}