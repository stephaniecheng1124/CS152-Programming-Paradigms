package value
import expression._
import context._

//BooleTest output:
//t && f = false
//t || f = true
//!t = false

//CharsTest output:
//s4 = California...Dreaming
//iforn
//California < Dreaming = -1
//California == California = true

//NumberTest1 output:
//i1 * i2 = 42
//i1 == i2 = false
//i1 < i2 = false
//i1.## = 55
//-r1 = -3.14
//r1 * r2 = 8.5094
//r1 == r2 = false
//r1 < r2 = false
//r1.## = 1565118
//r1 * i2 = 18.84
//i1 * r2 = 18.97

trait Value {
 }


class Notification(val ack: String) extends Value {
	def execute = ack
	override def toString = ack.toString
}
	
object Notification {	
	val OK = new Notification("OK")   //> ok  : value.Notification = value.Notification@18eed359
	val DONE = new Notification("DONE")                                            //> done  : value.Notification = value.Notification@4c40b76e
	val UNSPECIFIED = new Notification("UNSPECIFIED")
                                                  //> unspecified  : value.Notification = value.Notification@2ea6137
	def apply(ack: String) = new Notification(ack)               //> apply: (ack: String)value.Notification
}
	
class Variable extends Value {
}
	
class Store extends Value {
}
	
class Closure extends Value {
}
	
trait Literal extends Expression with Value {
	def execute(env: Environment) = this
}
		
case class Boole(val value: Boolean) extends Literal with Equals{
	def &&(other: Boole) = Boole(this.value && other.value)
	def ||(other: Boole) = Boole(this.value || other.value)
	def unary_! = Boole(!value)
	
	override def toString = value.toString	
}
	
	
case class Chars(val value: String) extends Literal with Ordered[Chars]{
	//<, ==, substring, and +
		
	def compare(other: Chars): Int = this.value.compare(other.value)
	def +(other: Chars) = Chars(this.value + other.value)
	
	def length: Integer = Integer(value.length)
	
	def substring(begin: Integer, end: Integer) = Chars(value.substring(begin.value, end.value))
		
	override def equals(other: Any): Boolean =
		other match {
		case other: Chars => other.isInstanceOf[Chars] && (other.value == this.value)
		case _ => false
		}
		
		override def toString = value.toString	
}
	
case class Integer(val value: Int) extends Literal with Ordered[Integer] with Equals {
  def +(other: Integer) = Integer(this.value + other.value)
	def *(other: Integer) = Integer(this.value * other.value)
	def -(other: Integer) = Integer(this.value - other.value)
	def /(other: Integer) = {
    if(other == 0) throw new Exception("Divide by zero error")
    else Integer(this.value / other.value)
	}
  def unary_- = Integer(-value) //Unary negation
  override def toString = value.toString
	def compare(other: Integer): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0
	override def canEqual(other: Any) =  other.isInstanceOf[Integer]
	override def equals(other: Any): Boolean =
	  other match {
	    case other: Integer => this.canEqual(other) && (other.value == this.value)
	    case _ => false
	  }
	override def hashCode = this.toString.##
}
	
	
object Integer {
	
	implicit def intToReal(n: Integer): Real = Real(n.value.toDouble)
}
		
case class Real(val value: Double) extends Literal with Ordered[Real] with Equals {
	def +(other: Real) = Real(this.value + other.value)
	def *(other: Real) = Real(this.value * other.value)
	def -(other: Real) = Real(this.value - other.value)
	def /(other: Real) = {
	  if(other == 0) throw new Exception("Divide by zero error")
	  	else Real(this.value / other.value)
	}
	def unary_- = Real(-value) //Unary negation
	override def toString = value.toString
	def compare(other: Real): Int = if (this.value < other.value) -1 else if (other.value < this.value) 1 else 0  
	override def canEqual(other: Any) =  other.isInstanceOf[Real]  
	override def equals(other: Any): Boolean =
	  other match {
	     case other: Real => this.canEqual(other) && (other.value == this.value)
	     case _ => false
	  }  
	override def hashCode = this.toString.##
}
  
