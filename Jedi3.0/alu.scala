package context
import value._
import expression._
import collection.mutable._

// *************************** COMPLETE THE ALU *******************************************************************************
/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)                             //Done 
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args) //binary          //2 operands        //Done
      case "more" => more(args) // binary         //2 operands
      case "equals" => equals(args) // note: equals(7, true) = false, not error
      case "unequals" => unequals(args) // binary, = not(equals(args))?     //2 operands
      case "not" => not(args) // unary     //1 operand 
      // primitive I/O ops:
      case "write" => write(args)                         //Done
      case "prompt" => prompt(args)                       //Done
      case "read" => read(args)                           //Done
      // variables
      case "dereference" => dereference(args)     //Done
      case "var" => makeVar(args)                 //Done
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
   // variable ops ***********************************************************
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = {
     if (args(0).isInstanceOf[Variable]) {
       args(0).asInstanceOf[Variable].content
     }
     else throw new Exception("Can't dereference a non-variable!")
   }
   
   // creates a new variable containing args(0)
   //?Don't need to make a new variable??
   private def makeVar(args: List[Value]) = {
     new Variable(args(0))
   }
   
   
   // store ops ***********************************************************
   
   //May test by printing out values in the store**
   // returns a new store containing args
   private def store(args: List[Value]) = {
     val elements = ArrayBuffer[Value]()
     elements ++= args
     new Store(elements)
   }
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: put(p: Integer, s: Store)")
     if(!args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(p: Integer, s: Store)")
      args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer]) 
      Notification.DONE
   }
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: get(p: Integer, s: Store)")
     if(!args(0).isInstanceOf[Integer] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: get(p: Integer, s: Store)")
      args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])   
   }
   
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: map(f:Closure, s: Store)")
     if(!args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: map(f:Closure, s: Store)")
      args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure]) 
   } 
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: filter(f:Closure, s: Store)")
     if(!args(0).isInstanceOf[Closure] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: filter(f:Closure, s: Store)")
      args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
   } 
   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: contains(v: Value, s: Store)")
     if(!args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: contains(f:Value, s: Store)")
      args(1).asInstanceOf[Store].contains(args(0).asInstanceOf[Value])
   }
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = {
     if (args.size != 2)
        throw new TypeException("expected signature: addLast(v: Value, s: Store)")
     if(!args(0).isInstanceOf[Value] || !args(1).isInstanceOf[Store]) 
        throw new TypeException("expected signature: addLast(f:Value, s: Store)")
     args(1).asInstanceOf[Store].add(args(0).asInstanceOf[Value])
     Notification.DONE
   }
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = {
     if (args.size != 1)
        throw new TypeException("expected signature: size(s: Store)")
     if(!args(0).isInstanceOf[Store]) 
        throw new TypeException("expected signature: size(s: Store)")
     args(0).asInstanceOf[Store].size
   }
   
  // etc.
  
  
  
  
    private def toInt(arg: Value): Option[Integer] =
      if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
      
    private def toReal(arg: Value): Option[Real] =
      if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
      else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
      else None
    private def toChars(arg: Value): Option[Chars] =
      if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None
      
    private def add(args: List[Value]): Value = {
      val args2 = args.map(toInt).filter(_ != None) //Get rid of everything that is not an int and empty
      if (args2.size == args.size) args2.flatten.reduce(_+_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_+_)
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) args4.flatten.reduce(_+_)
          else {
            throw new TypeException("Inputs to + must be numbers or texts")
          }
        }
      }
    }
  
  //*****
  private def mul(args: List[Value]): Value = {
      val args2 = args.map(toInt).filter(_ != None) //Get rid of everything that is not an int and empty
      if (args2.size == args.size) args2.flatten.reduce(_*_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_*_)
        else {
          throw new TypeException("Inputs to * must be numbers")
        }
      }
    }
  
  //*****
  private def sub(args: List[Value]): Value = {
    
      val args2 = args.map(toInt).filter(_ != None) //Get rid of everything that is not an int or is empty
     
      if (args2.size == args.size) args2.flatten.reduce(_-_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_-_)
        else {
          throw new TypeException("Inputs to - must be numbers")
        }
      }
    }
  
  //*****
  private def div(args: List[Value]): Value = {
      val args2 = args.map(toInt).filter(_ != None) //Get rid of everything that is not an int or is empty
      if (args2.size == args.size) args2.flatten.reduce(_*_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_*_)
        else {
          throw new TypeException("Inputs to * must be numbers")
        }
      }
    }
  
  def less(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("less expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) < args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) < args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) < args4(1))
          else throw new TypeException("Inputs to < must be numbers or texts")
        }
      }
   }  
 
  //*****
  def more(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("more expects two inputs")
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) Boole(args2(0) > args2(1))
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) Boole(args3(0) > args3(1))
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) Boole(args4(0) > args4(1))
          else throw new TypeException("Inputs to > must be numbers or texts")
        }
      }
   }  
 
  //*****
  def equals(args: List[Value]): Value = {
     
    
    
    val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) {
        val args2filt = args2.filter(_ == args2(0))
        if (args2filt.size == args2.size) Boole(true)
        else Boole(false) 
      }
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) {
           val args3filt = args3.filter(_ == args3(0))
        if (args3filt.size == args3.size) Boole(true)
        else Boole(false)    
        }
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) {
             val args4filt = args4.filter(_ == args4(0))
            if (args4filt.size == args4.size) Boole(true)
            else Boole(false) 
          }
          else Boole(false)
        }
      }
   } 
  
  //*****
   def unequals(args: List[Value]): Value = {
      if (args.length  != 2) throw new TypeException("unequals expects two inputs")
      not(List(equals(args)))   
   } 
  
  //*****
  def not(args: List[Value]): Value = {
    if (args.length  != 1) throw new TypeException("not expects 1 input")
    if (args(0).isInstanceOf[Boole]) !args(0).asInstanceOf[Boole]
    else {
      throw new TypeException("Should be of type Boole")
    }
      
  }
 
   def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
   def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
   def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

}
