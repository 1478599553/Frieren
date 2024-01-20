package frieren

import scala.util.parsing.combinator.*
import frieren.*

object FrierenParser extends RegexParsers {
    /*
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
    */

    /*
    <expression> ::= <int>
                 | <bool>
                 | <variable>
                 | <function-application>
                 | <if-expression>
                 | <let-expression>
    let f = do(re,mi);

    */
    def debug[T](p: Parser[T]): Parser[T] = p ^^{it =>
        println(it)
        it
    }

    def bracketed[T](p: Parser[T]): Parser[T] = spaced(p) | (spaced("(") ~> bracketed(spaced(p)) <~ spaced(")"))

    def expr : Parser[AstNode] = {
        println("expr")
        debug(bracketed(spaced(number) | spaced(bool) | spaced(let) | spaced(abstraction) | spaced(application) | spaced(symbol) ))
    }

    def number: Parser[AstNode] = {
        println("number")
        """-?\d+""".r ^^ (s => Number(s.toInt))
    }
    def symbol: Parser[Symbol] = {
        println("symbol")
        """([+*\-/=<>!]+)|([a-zA-Z_][a-zA-Z_1-9]*)""".r ^^ (s => Symbol(s))
    }
    def symbolList : Parser[List[Symbol]] = {
        println("symbolList")
        spaced("(" ~> rep(spaced(symbol)) <~ ")")
    }
    def spaced[T](p: Parser[T]): Parser[T] = debug(p <~ """\s*""".r)
    def bool: Parser[Bool] = {
        println("bool")
        "true" ^^ { _ => Bool(true)} | "false" ^^ { _ => Bool(false)}
    }

    def block : Parser[List[AstNode]] = {
        println("block")
        spaced("{") ~> rep(spaced(expr)) <~ spaced("}")
    }
    def exprOrBlock : Parser[List[AstNode]] = {
        println("exprOrBlock")
        spaced(expr) ^^ {it => List(it)} | block
    }

    def arguList : Parser[List[AstNode]] = {
        println("arguList")
        spaced("(") ~> spaced(repsep(spaced(expr),spaced(","))) <~ spaced(")")
    }

    def listList: Parser[List[List[AstNode]]] = {
        println("listList")
        spaced(arguList) ~ spaced(rep(spaced(arguList))) ^^ {case head ~ tail => tail.::(head)}
    }

    def exrp: Parser[AstNode] = {
        println("exrp")
        (spaced("(") ~> bracketed(spaced(expr)) <~ spaced(")")) | bracketed(spaced(number) | spaced(bool) | spaced(let) | spaced(abstraction) | spaced(symbol) )
    }

    def application : Parser[Apply] = {
        println("application")
        (spaced(exrp) ~ spaced(listList)) ^^{case func ~ lList =>
            var res: Apply = Apply(func, lList.head)
            lList.tail.foreach(it =>
                res = Apply(res, it)
            )
            res
        }
    }

    def paraList : Parser[List[Symbol]] = {
        println("paraList")
        spaced("(") ~> spaced(repsep(spaced(symbol),spaced(","))) <~ spaced(")") | (spaced(symbol)^^{it => List(it)})
    }
    def abstraction : Parser[Abstraction] = {
        println("abstraction")
        (spaced("fn") ~> spaced(paraList) ~ (spaced("=>") ~> (block | expr ^^ (it => List(it))))) ^^ { case para ~ body => Abstraction(para, body) }
    }
    // let x = 1 in x;
    // let (x = 1, y = 2) in x + y;
    def letBindings : Parser[List[(Symbol,AstNode)]] = {
        println("LetBindings")
        (spaced(symbol) ~ (spaced("=") ~> spaced(expr))) ^^ { case s ~ v => List((s, v)) }
            | (spaced("(") ~> spaced(repsep(spaced(symbol) ~ (spaced("=") ~> spaced(expr)), spaced(","))) <~ spaced(")")) ^^ (bindings => bindings.map({ case s ~ v => (s, v) }))
    }
    def let : Parser[Let] = {
        println("let")
        (spaced("let") ~> letBindings ~ (spaced("in") ~> spaced(exprOrBlock))) ^^ {case bindings ~ body => Let(bindings,body)}
    }
    
    
    def parseToAst(input: String): AstNode = parseAll(expr, input) match {
        case Success(result, _) => result
        case _ => throw new IllegalArgumentException("Parsing failed")
    }
}
