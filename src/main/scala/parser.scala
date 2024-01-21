package frieren

import scala.util.parsing.combinator.*
import frieren.*

object FrierenParser extends RegexParsers {
    def debug[T](p: Parser[T]): Parser[T] = p ^^{it =>
        println(it)
        it
    }

    def bracketed[T](p: Parser[T]): Parser[T] = spaced(p) | (spaced("(") ~> bracketed(spaced(p)) <~ spaced(")"))

    def expr : Parser[AstNode] = bracketed(spaced(number) | spaced(bool) | spaced(let) | spaced(application) | spaced(abstraction) | spaced(symbol) )

    def number: Parser[AstNode] = """-?\d+""".r ^^ (s => Number(s.toInt))
    def symbol: Parser[Symbol] = """([a-zA-Z_][a-zA-Z_1-9]*)""".r ^^ (s => Symbol(s))
    def symbolList : Parser[List[Symbol]] = spaced("(" ~> rep(spaced(symbol)) <~ ")")
    def spaced[T](p: Parser[T]): Parser[T] = p <~ """\s*""".r
    def bool: Parser[Bool] = "true" ^^ { _ => Bool(true)} | "false" ^^ { _ => Bool(false)}

    def block : Parser[List[AstNode]] = spaced("{") ~> rep(spaced(expr)) <~ spaced("}")
    def exprOrBlock : Parser[List[AstNode]] = spaced(expr) ^^ {it => List(it)} | block

    def arguList : Parser[List[AstNode]] = spaced("(") ~> spaced(repsep(spaced(expr),spaced(","))) <~ spaced(")")

    def listList: Parser[List[List[AstNode]]] = spaced(arguList) ~ spaced(rep(spaced(arguList))) ^^ {case head ~ tail => tail.::(head)}

    def exprApplication: Parser[AstNode] = (spaced("(") ~> bracketed(spaced(application)) <~ spaced(")")) | bracketed(spaced(let) | spaced(abstraction) | spaced(symbol) )

    def application : Parser[Apply] = (spaced(exprApplication) ~ spaced(listList)) ^^{case func ~ lList =>
            var res: Apply = Apply(func, lList.head)
            lList.tail.foreach(it =>
                res = Apply(res, it)
            )
            res
        }

    def paraList : Parser[List[Symbol]] = spaced("(") ~> spaced(repsep(spaced(symbol),spaced(","))) <~ spaced(")") | (spaced(symbol)^^{it => List(it)})
    def abstraction : Parser[Abstraction] = (spaced("fn") ~> spaced(paraList) ~ (spaced("=>") ~> (block | expr ^^ (it => List(it))))) ^^ { case para ~ body => Abstraction(para, body) }
    // let x = 1 in x;
    // let (x = 1, y = 2) in x + y;
    def letBindings : Parser[List[(Symbol,AstNode)]] = (spaced(symbol) ~ (spaced("=") ~> spaced(expr))) ^^ { case s ~ v => List((s, v)) }
            | (spaced("(") ~> spaced(repsep(spaced(symbol) ~ (spaced("=") ~> spaced(expr)), spaced(","))) <~ spaced(")")) ^^ (bindings => bindings.map({ case s ~ v => (s, v) }))
    def let : Parser[Let] = (spaced("let") ~> letBindings ~ (spaced("in") ~> spaced(exprOrBlock))) ^^ {case bindings ~ body => Let(bindings,body)}
    
    
    def parseToAst(input: String): AstNode = parseAll(expr, input) match {
        case Success(result, _) => result
        case _ => throw new IllegalArgumentException("Parsing failed")
    }
}
