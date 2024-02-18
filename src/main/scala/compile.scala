package frieren
def compile(expr:AstNode) : String = {
    expr match
        case Symbol(name) => name
        case Number(value) =>value.toString
        case Add(value1, value2) =>s"${compile(value1)}+${compile(value2)}"
        case Mul(lhs, rhs) =>s"${compile(lhs)}*${compile(rhs)}"
        case Abstraction(param, body) => s"[&](${param.map({ case Symbol(it) => it }).map(it => s"auto $it").mkString(",")}) {return ${compile(body)};}"
        case Apply(func, arg) =>s"(${compile(func)})(${arg.map(compile).mkString(",")})"
        case Let(bindings, in) =>
            s"""
               |
               |[&](){
               |${bindings.map({ case (Symbol(sym), value) => s"auto $sym = ${compile(value)};" }).mkString("")}
               |return ${compile(in)};
               |}()
               |""".stripMargin
        case Bool(value) =>value.toString
        case Block(content) => ???
        case Match(obj, arms) => ???

}

def toCppType(typ:Type): String = {
    typ match
        case Type.Arrow(left, right) =>
            (left,right) match
                case (Type.Var(_), Type.Var(_)) => ???
        case Type.Var(num) => ???
        case Type.RealType(name) =>
            name match
                case RType.Int => "int"
                case RType.Bool => "bool"
                case RType.Unit => "unit"
                case RType.WrongType(message) => ???
}
