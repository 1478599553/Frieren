package frieren

import scala.util.parsing.combinator.*
import frieren.*
import _root_.frieren.Pattern.{ConstructorDestruction, Identifier, WildCard}

object FrierenParser extends RegexParsers {
    def debug[T](p: Parser[T]): Parser[T] = p ^^{it =>
        println(it)
        it
    }

    def bracketed[T](p: Parser[T]): Parser[T] = (spaced("(") ~> bracketed(spaced(p)) <~ spaced(")")) | spaced(p)

    def bracket[T](p: Parser[T]): Parser[T] = spaced("(") ~> bracketed(spaced(p)) <~ spaced(")")
    def expr : Parser[AstNode] = spaced(matchexpr | let | abstraction | op(last) | bracketed(application) | bracketed(number) | bracketed(bool) | bracketed(block) | bracketed(data) | bracketed(symbol) | bracket(expr))

    def getOne : Parser[AstNode] = {
        spaced(bracketed(application) | bracketed(number) | bracketed(bool) | bracketed(block) | bracketed(symbol) | bracket(expr))
    }

    def first: Parser[AstNode => AstNode] = {
        getfuncs(spaced(mul))
    }

    def last : Parser[AstNode => AstNode] = {
        getfuncs(spaced(add))
    }

    def op(p : Parser[AstNode => AstNode]) : Parser[AstNode] = {
        spaced(getOne) ~ p ^^ {case arg ~ func => func(arg)} | spaced(getOne)
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
        spaced("+") ~> spaced(op(first)) ^^ {v2 => v1 => Add(v1, v2)}
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

    def arguList : Parser[List[AstNode]] = spaced("(") ~> spaced(repsep(spaced(expr),spaced(","))) <~ (spaced(")") | exception("\")\" not found in argument"))

    def listList: Parser[List[List[AstNode]]] = spaced(arguList) ~ spaced(rep(spaced(arguList))) ^^ {case head ~ tail => tail.::(head)}

    def exprApplication: Parser[AstNode] = (spaced("(") ~> bracketed(spaced(application)) <~ spaced(")")) | bracketed(spaced(let) | spaced(abstraction) | spaced(symbol) ) | bracket(expr)

    def application : Parser[Apply] = {
        (spaced(exprApplication) ~ spaced(listList)) ^^{case func ~ lList =>
            var res: Apply = Apply(func, lList.head)
            lList.tail.foreach(it =>
                res = Apply(res, it)
            )
            res
        }
    }

    def paraList : Parser[List[Symbol]] = spaced("(") ~> spaced(repsep(spaced(symbol),spaced(","))) <~ spaced(")") | (spaced(symbol)^^{it => List(it)})
    def abstraction : Parser[Abstraction] = {
        (spaced("fn") ~> spaced(paraList) ~ ((spaced("=>") | exception("fn without =>")) ~> expr )) ^^ { case para ~ body => Abstraction(para, body) }
    }
    
    def letBindings : Parser[List[(Symbol,AstNode)]] = (spaced(symbol) ~ (spaced("=") ~> spaced(expr))) ^^ { case s ~ v => List((s, v)) }
            | (spaced("(") ~> spaced(repsep(spaced(symbol) ~ (spaced("=") ~> spaced(expr)), spaced(","))) <~ spaced(")")) ^^ (bindings => bindings.map({ case s ~ v => (s, v) }))
    def let : Parser[Let] = (spaced("let") ~> letBindings ~ ((spaced("in") ~> spaced(expr)) | exception("let without in expr"))) ^^ {case bindings ~ body => Let(bindings,body)}
    //data shape = Circle of int * int
    def constructor : Parser[(String,List[String])] = spaced(symbol) ~ (spaced("of") ~> repsep(spaced(symbol),spaced("*"))) ^^ {case Symbol(constructor) ~ paraTypeList => (constructor,paraTypeList.map({case Symbol(name)=>name}))}
    def data : Parser[Data] = (spaced("data") ~> spaced(symbol) ~ (spaced("=") ~> repsep(spaced(constructor),spaced("|"))) ^^ {case Symbol(name) ~ con => Data(name,con)})

    def matchexpr : Parser[Match] = (spaced("match") ~> spaced(symbol) <~ (spaced("with") | exception("match without with"))) ~ rep(spaced(pattern) ~ spaced(expr) ^^ {case p ~ e => (p, e)}) ^^ {case obj ~ arms => Match(obj, arms)}

    def pattern : Parser[Pattern] = spaced("|") ~> spaced("_") <~ (spaced("->") | exception("pattern without ->")) ^^ (_ => WildCard) |
        spaced("|") ~> spaced("""([a-zA-Z_][a-zA-Z_1-9]*)""".r) ~ bracket(repsep(spaced("""([a-zA-Z_][a-zA-Z_1-9]*)""".r), spaced(","))) <~ (spaced("->") | exception("pattern without ->")) ^^ {case constructor ~ items => ConstructorDestruction(constructor, items)} |
        spaced("|") ~> spaced("""([a-zA-Z_][a-zA-Z_1-9]*)""".r) <~ (spaced("->") | exception("pattern without ->")) ^^ (s => Identifier(s))
    def exception(message: String): Parser[AstNode] = {
        throw ParserException(message)
    }
    private class ParserException(message: String) extends Exception(message)
    def parseToAst(input: String): AstNode =
        try {
            parseAll(expr, input) match {
                case Success(result, _) => result
                case _ => throw new IllegalArgumentException("Parsing failed")
            }
        }catch
            case w:ParserException => throw new IllegalArgumentException(s"Parsing failed: ${w.getMessage}")

}