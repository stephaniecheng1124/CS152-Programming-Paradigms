package expression
import value._
import context._
import scala.util.control.Breaks._

//ExpTest Output:
//3.14
//3.14

trait Expression {
	def execute(env: Environment): Value 
}
	
case class Identifier(val name: String) extends Expression {
	 override def toString = name
   def execute(env: Environment) = env(this)
}

case class FunCall(operator: Expression, operands: List[Expression]) extends Expression {
  
  def execute(env: Environment): Value = {
    var argList = List[Value]()
    for (exp <- operands) {
      argList = argList :+ exp.execute(env)
    }
    if (operator.isInstanceOf[Identifier]) {
      alu.execute(operator.asInstanceOf[Identifier], argList)
    }
    else Notification.UNSPECIFIED
  }

}

trait SpecialForm extends Expression {

}

case class Conditional(cond: Expression, consequence: Expression, alt: Expression = null) extends SpecialForm {
   def execute(env: Environment): Value = {
       val trueFalse = cond.execute(env)
       if (trueFalse.isInstanceOf[Boole]) {
         val booleTF = trueFalse.asInstanceOf[Boole]
         if (booleTF.value) {
           consequence.execute(env)
         }
         else {
           if (alt != null) alt.execute(env)
           else Notification.UNSPECIFIED
         }
       }
       else {
         throw new Exception("Condition has to be of type Boolean!")
       }
   }
}

case class Disjunction(listConj: List[Expression]) extends SpecialForm {
   def execute(env: Environment): Value = {
     var answer = Boole(false)
     breakable {
         
       for (con <- listConj) {
         val trueFalse = con.execute(env)
         if (trueFalse.isInstanceOf[Boole]) {
           val booleTF = trueFalse.asInstanceOf[Boole]
           if (booleTF.value) {
             answer = Boole(true)
             break
           }
         }
       }
         
     } 
      
     answer
     
	 }
}

case class Conjunction(listEqual: List[Expression]) extends SpecialForm {

   def execute(env: Environment): Value = {
     var answer = Boole(true)
     breakable {
         
       for (eql <- listEqual) {
         val trueFalse = eql.execute(env)
         if (trueFalse.isInstanceOf[Boole]) {
           val booleTF = trueFalse.asInstanceOf[Boole]
           if (!booleTF.value) {
             answer = Boole(false)
             break
           }
         }
       }
         
     }    
     answer     
	 }
     
}

case class Declaration(key: Identifier, value: Expression) extends SpecialForm {
  
  def execute(env: Environment): Value = {
    env.put(key, value.execute(env))
    Notification.OK
  }
  
}


