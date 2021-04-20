package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~ opt(identifier ~ rep("," ~> identifier)) ~ ")" ^^ {
   case "(" ~ None ~ ")" => List[Identifier]()
   case "(" ~ Some(iden ~ Nil) ~ ")" => List(iden)
   case "(" ~ Some(iden ~ more) ~ ")" => iden::more
   case _ => Nil
  }
 
 
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~ params ~ expression ^^ {
   case "lambda" ~ paramList ~ exp => Lambda(paramList, exp)
  }
  
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~ expression ~ rep(";" ~> expression) ~ "}" ^^ {
   case "{" ~ exp1 ~ moreExp ~ "}" => Block(exp1::moreExp)
  }
  
  // override of term parser
  
  override def term: Parser[Expression]  = lambda | funCall | block | literal | "("~>expression<~")"
}
