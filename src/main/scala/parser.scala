package frieren

import scala.util.parsing.combinator._

sealed trait AstNode
case class Symbol(name: String) extends AstNode
case class Number(value: Int) extends AstNode
case class Add(value1: AstNode, value2: AstNode) extends AstNode
case class Abstraction(param: List[Symbol], body: List[AstNode]) extends AstNode
case class Apply(func: AstNode, arg: List[AstNode]) extends AstNode
case class Let()

object LispParser extends RegexParsers {
    
    def number: Parser[AstNode] = """-?\d+""".r ^^ (s => Number(s.toInt))
    def symbol: Parser[Symbol] = """([+*\-/=<>!]+)|([a-zA-Z_][a-zA-Z_1-9]*)""".r ^^ (s => Symbol(s))
    def symbolList : Parser[List[Symbol]] = spaced("(" ~> rep(symbol) <~ ")")
    def spaced[T](p: Parser[T]): Parser[T] = p <~ """\s*""".r

    def expr: Parser[AstNode] = spaced(number) | spaced(symbol) | spaced(addExpr) | spaced(abstractionExpr) | spaced(applyExpr)//spaced("(" ~> symbol ~ rep(expr) <~ ")" ^^ (list => list))

    def addExpr: Parser[Add] = "(" ~> "+" ~> expr ~ expr <~ ")" ^^ {
        case left ~ right => Add(left, right)
    }

    def para = symbol ^^ (item => List(item)) | symbolList
    def abstractionExpr: Parser[Abstraction] = spaced("(" ~> "lambda" ~> spaced(para) ~ rep(expr) <~ ")" ^^ {
        case para ~ body => Abstraction(para,body)
    })

    def applyExpr : Parser[Apply] = "(" ~> expr ~ rep(expr) <~ ")" ^^ {
        case func ~ arg => Apply(func, arg)
    }

    def parseToAst(input: String): AstNode = parseAll(expr, input) match {
        case Success(result, _) => result
        case _ => throw new IllegalArgumentException("Parsing failed")
    }
}
