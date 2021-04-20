object string {
  import scala.util.Random
  
  //Problem 1
  def isPal(x: String) = {
  		val word = x.trim
  		val charArray = word.split("")
  		val charArrayReversed = charArray.reverse
  		val flippedWord = charArrayReversed.mkString("")
  		
  		if (flippedWord == word) true
  		else false
  }                                               //> isPal: (x: String)Boolean
  
	isPal("rotator")                          //> res0: Boolean = true
	isPal("cat")                              //> res1: Boolean = false
	isPal("Rotator")                          //> res2: Boolean = false
	isPal("   rotator ")                      //> res3: Boolean = true
	isPal("r0tat0r")                          //> res4: Boolean = true
	isPal("racecar")                          //> res5: Boolean = true

  
  //Problem 2
  def isPal2(x: String) = {
  		val filterLowercase = x.replaceAll("[^a-zA-Z0-9]", "").toLowerCase
  		val charArray = filterLowercase.split("")
  		val charArrayReversed = charArray.reverse
  		val flippedWord = charArrayReversed.mkString("")
  		
  		if (flippedWord == filterLowercase) true
  		else false
  
  }                                               //> isPal2: (x: String)Boolean
  
  
  isPal2("Hi, I am BoB!")                         //> res6: Boolean = false
  isPal2("A man, a plan, a canal, Panama!")       //> res7: Boolean = true
  
  //Problem 4
  def mkWord(length: Int = scala.util.Random.nextInt(3) + 4) = {

  		val randomWord = Random.alphanumeric.filter(_.isLetter).take(length).mkString.toLowerCase
  		
  		randomWord
  }                                               //> mkWord: (length: Int)String
  
  mkWord()                                        //> res8: String = xjfbk
  mkWord()                                        //> res9: String = awmuh
  mkWord()                                        //> res10: String = hgmzcx
  mkWord()                                        //> res11: String = hpvpb
  mkWord(10)                                      //> res12: String = xgpcmbvtrm
  
  //Problem 5
  def mkSentence(sentLength: Int = scala.util.Random.nextInt(8) + 3) = {
  		def mkWord(begMidEnd: Int) = {
  			val wordLength = scala.util.Random.nextInt(7) + 1
  			if (begMidEnd == 0) {
  				val firstLetter = Random.alphanumeric.filter(_.isLetter).take(1).mkString.toUpperCase
  				val randomWord = Random.alphanumeric.filter(_.isLetter).take(wordLength).mkString.toLowerCase
  				val firstWord = firstLetter + randomWord
  				firstWord
  			}
  			else if (begMidEnd == 1) {
  				val randomWord = Random.alphanumeric.filter(_.isLetter).take(wordLength).mkString.toLowerCase
  				randomWord
  			}
  			else {
  				val randomWord = Random.alphanumeric.filter(_.isLetter).take(wordLength).mkString.toLowerCase
  				val lastWord = randomWord + "."
  				lastWord
  			}
		}
  			
  		val middleWordsArray = new Array[String](sentLength - 2)
  		for(i <- 0 until sentLength - 2) {
   			middleWordsArray(i) = mkWord(1)
  		}
  		val finalSentence = mkWord(0) + " " + middleWordsArray.mkString(" ") + " " + mkWord(2)
		finalSentence
  }                                               //> mkSentence: (sentLength: Int)String
  
  mkSentence()                                    //> res13: String = Ibizhyiu uy wq bqrnoa sr wffefvg nwtqxn vkd u.
  mkSentence()                                    //> res14: String = Shpre vtgle h ttzubx twkm kt opkp tuwk rb.
  mkSentence()                                    //> res15: String = Mixdaqi gbcd sha hdthfaz sc ixonb vajgxq xqvwhbx.
  mkSentence(5)                                   //> res16: String = Krajjfyn exuvuzn ocs rm zjqdjtv.
  
  
  //Problem 8
  def eval(startExp: String) = {
  		def isNumeric(c: Char) = c.isDigit || c == '.' || c == '-'
  		def isOperator(c: Char) = c == '+'
  		def isSpace(c: Char) = c == ' ' || c == '\t' || c == '\n'
  
		var exp = startExp.dropWhile(isSpace _)
		
		val num1 = exp.takeWhile(isNumeric)
		if (num1.isEmpty) throw new Exception("NumberFormatException")
			
  		exp = exp.drop(num1.length)
  		exp = exp.dropWhile(isSpace)
  		
  		val operator = exp.takeWhile(isOperator)
  		if (operator.isEmpty) throw new Exception("missing operator")
  		
  		exp = exp.drop(operator.length)
  		exp = exp.dropWhile(isSpace)
  		
  		val num2 = exp.takeWhile(isNumeric)
		if (num2.isEmpty) throw new Exception("NumberFormatException")
  	
  	
  		num1.toDouble + num2.toDouble

  }                                               //> eval: (startExp: String)Double
  
  eval("3.14+42")                                 //> res17: Double = 45.14
	eval("  -26  +  -49.99  ")                //> res18: Double = -75.99000000000001
  try {
  		eval("21 * 43")
  } catch {
  		case e: Exception => println(e)
  }                                               //> java.lang.Exception: missing operator
                                                  //| res19: AnyVal = ()
  try {
		eval("abc + 3")
  } catch {
  		case e: Exception => println(e)
  }                                               //> java.lang.Exception: NumberFormatException
                                                  //| res20: AnyVal = ()
   
  
  //Problem 9
  
 	  def eval2(startExp: String) = {
  		def isNumeric(c: Char) = c.isDigit || c == '.' || c == '-'
  		def isOperator(c: Char) = c == '+' || c == '*'
  		def isSpace(c: Char) = c == ' ' || c == '\t' || c == '\n'
  
		var exp = startExp.dropWhile(isSpace _)
		
		val num1 = exp.takeWhile(isNumeric)
		if (num1.isEmpty) throw new Exception("NumberFormatException")
				
  		exp = exp.drop(num1.length)
  		exp = exp.dropWhile(isSpace)
  		
  		val operator = exp.takeWhile(isOperator)
  		if (operator.isEmpty) throw new Exception("missing operator")
  		
  		exp = exp.drop(operator.length)
  		exp = exp.dropWhile(isSpace)
  		
  		val num2 = exp.takeWhile(isNumeric)
		if (num2.isEmpty) throw new Exception("NumberFormatException")
  	
  		if (operator == "*") num1.toDouble * num2.toDouble
  		else num1.toDouble + num2.toDouble

  }                                               //> eval2: (startExp: String)Double
  
  eval2("3.14+42")                                //> res21: Double = 45.14
  eval2("3.14* 42")                               //> res22: Double = 131.88
  eval2("  -26  +  -49.99  ")                     //> res23: Double = -75.99000000000001
  eval2("  -26  *  -49.99  ")                     //> res24: Double = 1299.74
  
  try {
  		eval2("21 ^ 43")
  } catch {
  		case e: Exception => println(e)
  }                                               //> java.lang.Exception: missing operator
                                                  //| res25: AnyVal = ()
  try {
		eval2("4 * abc")
  } catch {
  		case e: Exception => println(e)
  }                                               //> java.lang.Exception: NumberFormatException
                                                  //| res26: AnyVal = ()
  

  
  
  
  

}