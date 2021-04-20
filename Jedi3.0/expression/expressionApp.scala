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
   def execute(env: Environment) = env.apply(this)
}

case class FunCall(operator: Expression, operands: List[Expression]) extends Expression {
  
  def execute(env: Environment): Value = {
    
    var argList = List[Value]()
    for (exp <- operands) {
      argList = argList :+ exp.execute(env)
    }
    
    if (operator.isInstanceOf[Identifier]) {
      val opIdentifier = operator.asInstanceOf[Identifier]
      
      if (env.extHas(opIdentifier, env) && env(opIdentifier).isInstanceOf[Closure] ) {
       
        val closure = env(opIdentifier).asInstanceOf[Closure]
        closure.apply(argList)
      }
      else {
        alu.execute(operator.asInstanceOf[Identifier], argList)     
      }
    }
    else Notification.UNSPECIFIED
  }

}

trait SpecialForm extends Expression {

}

//Jedi 3.0
case class Assignment(vbl: Identifier, update: Expression) extends SpecialForm {
   
  def execute(env: Environment): Value = {
    if (env.extHas(vbl,env) && env(vbl).isInstanceOf[Variable]) {
      val theVar = env(vbl).asInstanceOf[Variable]
      theVar.content = update.execute(env)       
      Notification.DONE
    }
    else throw new Exception("Assignment could not be made to a non-variable")
  }
  
}

//Jedi 3.0*****************
case class Iteration(condition: Expression, body: Expression) extends SpecialForm {
   
  def execute(env: Environment): Value = {
    if (condition.execute(env).isInstanceOf[Boole]) {
      
      var truefalse = condition.execute(env).asInstanceOf[Boole]
      
      while(truefalse.asInstanceOf[Boole].value) {  
        body.execute(env)
        truefalse = condition.execute(env).asInstanceOf[Boole]
      }
      Notification.OK
    }
    else throw new Exception("Condition is not valid!")
  }
  
}


//later do some type checking maybe
case class Block(listExp: List[Expression]) extends SpecialForm {
   
  def execute(env: Environment): Value = {
    val tempEnv = new Environment(env)
    for (i <- 0 to listExp.size - 2) {
    listExp(i).execute(tempEnv)
    } 
  listExp(listExp.size - 1).execute(tempEnv)
  }
 
  
}


//returns a closure
case class Lambda(val params: List[Identifier], val body: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    new Closure(params, body, env)
  }
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


