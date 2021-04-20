package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  //Variable Parsers********
  
  //Parser for making var?? //Created by me
   def variable: Parser[Expression] = "var" ~ operands  ^^ {
   case "var" ~ content => FunCall(Identifier("makeVar"), content)
  }
  
  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }

  // dereference ::= "[" ~ expression ~ "]"
  def dereference: Parser[Expression] = "[" ~ expression ~ "]" ^^ {
    case "[" ~ id ~ "]" => FunCall(Identifier("dereference"), List(id))
  }
  
  //Store parsers***********
   
  //Parser for making store? //Created by me
   def store: Parser[Expression] = "store" ~ operands  ^^ {
     case "store" ~ content => FunCall(Identifier("store"), content)
   }
   
  
   def put: Parser[Expression] = "put" ~ operands ^^ {
    case "put" ~ ops => FunCall(Identifier("put"), ops) 
   }
   def rem: Parser[Expression] = "rem" ~ operands ^^ {
    case "rem" ~ ops => FunCall(Identifier("rem"), ops) 
   }
   def contains: Parser[Expression] = "contains" ~ operands ^^ {
    case "contains" ~ ops => FunCall(Identifier("contains"), ops) 
   }
   def map: Parser[Expression] = "map" ~ operands ^^ {
    case "map" ~ ops => FunCall(Identifier("map"), ops) 
   }
   def filter: Parser[Expression] = "filter" ~ operands ^^ {
    case "filter" ~ ops => FunCall(Identifier("filter"), ops) 
   }
   def get: Parser[Expression] = "get" ~ operands ^^ {
    case "get" ~ ops => FunCall(Identifier("get"), ops) 
   }
   def addLast: Parser[Expression] = "addLast" ~ operands ^^ {
    case "addLast" ~ ops => FunCall(Identifier("addLast"), ops) 
   }
   def size: Parser[Expression] = "size" ~ operands ^^ {
    case "size" ~ ops => FunCall(Identifier("size"), ops) 
   }
  
     
   
   //Iteration parsers*********
   
   // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => Iteration(exp1, exp2)
  }

  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}