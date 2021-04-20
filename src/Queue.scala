//Problem 2: Generics

//Output:
//(Bob, Tom, Sam, Alex, Jeff)(Tom, Sam, Alex, Jeff)
//(Sam, Alex, Jeff)
//(Alex, Jeff)
//(Jeff)
//()


import scala.collection.mutable.ArrayBuffer
 

class Queue[T] (val capacity: Int = 100) {
	private val elems: ArrayBuffer[T] = new ArrayBuffer[T](capacity)
	  
	def enqueue(elem: T) {
	 	elems += elem
	}
	  
	def dequeue() {
	  elems -= elems(0)
	}
	  
	def isEmpty() = if (elems.size < 1) true else false
	  
	override def toString() = {
	 if (!isEmpty) {
	  	var inside = ""
	  	for (i <- 0 to elems.size - 2) {
	  	  inside += elems(i) + ", "		  		
	  	}
		"(" + inside + elems(elems.size - 1) + ")"
	  	}
	 else "()"
	 }

}
  
object Queue {
  	
 def main(args: Array[String]) : Unit = {
  	val waitingList = new Queue[String]
  	waitingList.enqueue("Bob")
  	waitingList.enqueue("Tom")
  	waitingList.enqueue("Sam")
 	waitingList.enqueue("Alex")
 	waitingList.enqueue("Jeff")
  		
  	print(waitingList)
  			
  	while (!waitingList.isEmpty) {
  		waitingList.dequeue
  		println(waitingList)
  	}
  			
 	}
 
  	def apply(capacity: Int = 100) = new Queue(capacity)
}
  

  
