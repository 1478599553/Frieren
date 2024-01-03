package frieren

import scala.util.parsing.combinator._

sealed trait AstNode
case class Symbol(name: String) extends AstNode
case class Number(value: Integer) extends AstNode
case class Add(value1: AstNode, value2: AstNode) extends AstNode
case class Abstraction(param: List[Symbol], body: List[AstNode]) extends AstNode
case class Apply(func: AstNode, arg: List[AstNode]) extends AstNode


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

object Test extends App {
    val caseList = List(
        "(lambda x (lambda f (lambda g (g (f x) (f x x)))))",
        "(lambda x ((lambda (f x) (f x)) x))",
        "(lambda (f x a) (f x (x a)))",
        "(lambda (a b c d e) ((lambda (a b f g h) (a b c d e f g h))))",
        "(lambda x (+ x 1))",
        "((lambda x (x x)) (lambda x (x x)))",
        "(+ 1 ((lambda x (+ x 2)) 3))",
        "((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x))",
        "((lambda f (f (lambda (x y) x) (lambda (x y) x))) (lambda (x y) ((x (lambda x x) 1) (y 2 3))))",
        "(lambda (f g x) (f x (g x)))",
        "((lambda (f g x) (f x (g x))) ((lambda (x y) x) ((lambda (f g x) (f x (g x))) ((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x)))) (lambda (x y) x))",
        "((lambda (s1 k1 s2 i k2) (s1 (k1 (s2 i)) k2)) (lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (f g x) (f x (g x))) ((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x)) (lambda (x y) x))",
        "(lambda x (lambda y y))",
        "(lambda a (lambda b (lambda f (f (a b)))))",//λ a. λ b. λ f. f a b
        "(lambda x1 (x1 x1))",


    )
    
    caseList.foreach(it =>
        println(s"$it => ${LispParser.parseToAst(it)}")
        println(s"${infer(LispParser.parseToAst(it))}")
    )
}


