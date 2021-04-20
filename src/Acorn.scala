//Problem 4: Acorn

//Output of TestAcorn: 
//the value of (+ 42.0 (* 3.14 2.71)) = 50.5094
//the value of (* 2.0 (* 3.0 5.0)) = 30.0

 	abstract class Expression {
   def execute: Double
	}
	
	class Sum(val operand1: Expression, val operand2: Expression) extends Expression {
	  def execute = operand1.execute + operand2.execute
	  override def toString = "(+ " + operand1 + " " + operand2 + ")"
	}
	object Sum {
	  def apply(operand1: Expression, operand2: Expression) =
	      new Sum(operand1, operand2)
	}
			
		
	class Product(val operand1: Expression, val operand2: Expression) extends Expression {
	  def execute = operand1.execute * operand2.execute
	  override def toString = "(* " + operand1 + " " + operand2 + ")"
	}
	object Product{
	  def apply(operand1: Expression, operand2: Expression) =
	      new Product(operand1, operand2)
	}
		
		
	class Number(val value: Double) extends Expression {
	  def execute = value
	  override def toString = value.toString
	}
	object Number {
	  def apply(value: Double) = new Number(value)
	}
	
	
  
  
  
  
  
  
  
