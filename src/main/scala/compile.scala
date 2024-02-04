package frieren

def compile(expr:AstNode) : String = {
    expr match
        case Symbol(name) => name
        case Number(value) => value.toString
        case Add(value1, value2) => s"${compile(value1)}+${compile(value2)}"
        case Mul(lhs, rhs) => s"${compile(lhs)}*${compile(rhs)}"
        case Abstraction(param, body) => s"|${param.map({ case Symbol(it) => it }).mkString(",")}|{${compile(body)}}"
        //s"[=](${param.map({ case Symbol(it) => it }).map(it => s"auto $it").mkString(",")}) {return ${compile(body)};}"
        case Apply(func, arg) => s"(${compile(func)})(${arg.map(compile).mkString(",")})"
        case Let(bindings, in) =>
            s"""
              |{
              |${bindings.map({ case (Symbol(name), v) => s"let $name = ${compile(v)};"}).mkString("")}
              |${compile(in)}
              |}
              |""".stripMargin
        case Bool(value) => value.toString
        case Block(content) => ???
}