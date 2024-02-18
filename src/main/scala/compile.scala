package frieren

import scala.collection.mutable.ListBuffer

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
        case Match(obj, arms) =>
            s"""
               |${arms.map({case (p,v) =>{
                p match
                    case Pattern.ConstructorDeconstruction(constructor, tupleItems) => ???
                    case Pattern.Identifier(ident) => ???
                    case Pattern.WildCard => ???
            } })}
               |""".stripMargin
        case Data(name,constructors) =>
            s"""
               |struct $name {
               |short flag;
               |union {
               |${constructors.map(
                {case (name,member)=>
                    s"""
                       |struct {
                       |${member.map(typename => s"$typename ${genName()};\n").mkString}
                       |}$name;
                       |""".stripMargin
                }
            ).mkString}
               |    }v;
               |};
               |${constructors.map({case (cName,member) =>
                val paraList = ListBuffer.empty[String]
            s"""
                   |auto $cName = [&](${member.map(it => {
                val pName = genName();
                paraList.addOne(pName);
                s"$it $pName"
            }).mkString(",")}){
                   |        return $name{${constructors.map({ case (consName, _) => consName }).indexOf(cName)},{.$cName={ ${paraList.mkString(",")} }}};
                   |    };
                   |""".stripMargin
            })}
               |""".stripMargin

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

var count = 0
def genName(): String = {
    count += 1
    "v"+count
}