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

    def bracketed[T](p: Parser[T]): Parser[T] = (spaced("(") ~> bracketed(spaced(p)) <~ spaced(")")) | spaced(p)

    def expr : Parser[AstNode] = spaced(bracketed(let) | bracketed(abstraction) | bracketed(application) | bracketed(last) | bracketed(number) | bracketed(bool) | bracketed(block) | bracketed(symbol))

    def getOne : Parser[AstNode] = {
        (spaced("(") ~> spaced(expr) <~ spaced(")")) | spaced(bracketed(let) | bracketed(abstraction) | bracketed(application) | bracketed(number) | bracketed(bool) | bracketed(block) | bracketed(symbol))
    }

    def first: Parser[AstNode] = {
        (spaced(getOne) ~ getfuncs(spaced(mul))) ^^{case x ~ func => func(x)} | spaced(getOne)
    }

    def last : Parser[AstNode] = {
        (spaced(first) ~ getfuncs(spaced(add))) ^^{case x ~ func => func(x)} | first
    }
    def getfuncs(p : Parser[AstNode => AstNode]) : Parser[AstNode => AstNode] = {
        (p ~ rep(p)) ^^{case head ~ tail =>
            x =>
                var res: AstNode = head(x)
                tail.foreach(func => {
                    res = func(res)
                })
                res
        }
    }
    def add: Parser[AstNode => Add] = {
        spaced("+") ~> spaced(first) ^^ {v2 => v1 => Add(v1, v2)}
    }

    def mul: Parser[AstNode => Mul] = {
        spaced("*") ~> spaced(getOne) ^^ { v2 => v1 => Mul(v1, v2) }
    }

    def number: Parser[AstNode] = """-?\d+""".r ^^ (s => Number(s.toInt))
    def symbol: Parser[Symbol] = """([a-zA-Z_][a-zA-Z_1-9]*)""".r ^^ (s => Symbol(s))
    def symbolList : Parser[List[Symbol]] = spaced("(" ~> rep(spaced(symbol)) <~ ")")
    def spaced[T](p: Parser[T]): Parser[T] = p <~ """\s*""".r
    def bool: Parser[Bool] = "true" ^^ { _ => Bool(true)} | "false" ^^ { _ => Bool(false)}

    def block : Parser[Block] = spaced("{") ~> (repsep(spaced(expr),spaced(";")) ^^ {it => Block(it)} ) <~ spaced("}")

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
    def abstraction : Parser[Abstraction] = {
        (spaced("fn") ~> spaced(paraList) ~ (spaced("=>") ~> expr )) ^^ { case para ~ body => Abstraction(para, body) }
    }
    // let x = 1 in x;
    // let (x = 1, y = 2) in x + y;
    def letBindings : Parser[List[(Symbol,AstNode)]] = (spaced(symbol) ~ (spaced("=") ~> spaced(expr))) ^^ { case s ~ v => List((s, v)) }
            | (spaced("(") ~> spaced(repsep(spaced(symbol) ~ (spaced("=") ~> spaced(expr)), spaced(","))) <~ spaced(")")) ^^ (bindings => bindings.map({ case s ~ v => (s, v) }))
    def let : Parser[Let] = (spaced("let") ~> letBindings ~ (spaced("in") ~> spaced(expr))) ^^ {case bindings ~ body => Let(bindings,body)}
    
    
    def parseToAst(input: String): AstNode = parseAll(expr, input) match {
        case Success(result, _) => result
        case _ => throw new IllegalArgumentException("Parsing failed")
    }
}
