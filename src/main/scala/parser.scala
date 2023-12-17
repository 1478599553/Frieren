package frieren

import scala.util.parsing.combinator._


sealed trait LispValue
case class LispNumber(value: Int) extends LispValue
case class LispSymbol(name: String) extends LispValue
case class LispList(elements: List[LispValue]) extends LispValue
case class LispLambda(parameter: LispSymbol, body: LispValue) extends LispValue


sealed trait AstNode
case class Symbol(name: String) extends AstNode
case class Abstraction(param: Symbol, body: AstNode) extends AstNode
case class Apply(func: AstNode, arg: AstNode) extends AstNode


object LispParser extends RegexParsers {
    
    def number: Parser[LispNumber] = """-?\d+""".r ^^ (s => LispNumber(s.toInt))
    def symbol: Parser[LispSymbol] = """[a-zA-Z+*\-/=<>!]+""".r ^^ (s => LispSymbol(s))

    
    def spaced[T](p: Parser[T]): Parser[T] = p <~ """\s*""".r

    
    def expr: Parser[LispValue] = spaced(number) | spaced(symbol) | spaced(lambdaExpr) | spaced("(" ~> rep(expr) <~ ")" ^^ (list => LispList(list)))

    
    def lambdaExpr: Parser[LispLambda] = spaced("(" ~> "lambda" ~> symbol ~ expr <~ ")" ^^ { 
        case param ~ body => LispLambda(param, body)
    })

    
    def parseToAst(input: String): AstNode = parseAll(expr, input) match {
        case Success(result, _) => convertToAst(result)
        case _ => throw new IllegalArgumentException("Parsing failed")
    }

    // 辅助函数：将 Lisp AST 转换为抽象语法树
    def convertToAst(lispValue: LispValue): AstNode = lispValue match {
        case LispSymbol(name) => Symbol(name)
        case LispLambda(LispSymbol(param), body) => Abstraction(Symbol(param), convertToAst(body))
        case LispList(List(LispSymbol("lambda"), param: LispSymbol, body)) => Abstraction(Symbol(param.name), convertToAst(body))
        case LispList(List(func, arg)) => Apply(convertToAst(func), convertToAst(arg))
        case _ => throw new IllegalArgumentException(s"Unsupported Lisp value: $lispValue")
    }
}

object Test extends App {
    
    val caseList = List(
        "(lambda x (lambda y y))",
        "((lambda x (x x)) (lambda x (x x)))",
        "(lambda x (lambda y x))",
        "(lambda x (lambda y y))",
        "(lambda f (lambda x x))",// λ f. λ x. x
        "(lambda a (lambda b (lambda f (lambda f (a b)))))"//λ a. λ b. λ f. f a b
    )
    
    caseList.foreach(it => println(s"$it => ${LispParser.parseToAst(it)}"))
}
