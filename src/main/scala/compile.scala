package frieren

import java.util.UUID
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
var dataDefList = ListBuffer.empty[Data]
var isStruct = ListBuffer.empty[String]
var zeroParaConstructors = ListBuffer.empty[String]

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
        case Block(content) =>
            s"""
               |[&](){
               |${
                    val compiled = content.map(compile)
                    s"${compiled.init.mkString(";\n")} return ${compiled.last};"
                }
               |}()
               |""".stripMargin
        case Match(obj, arms) =>
            val objName = "v"+UUID.randomUUID().toString.replace("-","")
            s"""
               |[&](){
               |auto $objName = ${compile(obj)};
               |${arms.map {case (p,v) =>
                s"""
                   |if(${getPatternCond(p)(objName)}){
                   |${getPatternAssign(p)(objName).mkString}
                   |return ${compile(v)};
                   |}
                   |
                   |""".stripMargin
                }.mkString("else ")}
               |}()
               |""".stripMargin
        case data@Data(name,constructors) =>
            dataDefList.addOne(data)
            data.constructors.foreach{case (consName,cons)=>if cons.isEmpty then zeroParaConstructors.addOne(consName)}
            data.constructors.map { case (_, members) => members }.foreach(list=>list.foreach(it=>if dataDefList.map { case Data(name, _) => name }.contains(it) then isStruct.addOne(it)))

            s"""
               |struct $name {
               |short flag;
               |union {
               |${constructors.map(
                {case (name,member)=>
                    s"""
                       |struct {
                       |${member.zipWithIndex.map({ case (typename,index) => s"${if isStruct.contains(typename) then typename+"*" else typename} v$index;\n" }).mkString}
                       |}$name;
                       |""".stripMargin
                }
            ).mkString}
               |    }v;
               |};
               |${constructors.map({case (cName,member) =>
                if member.nonEmpty then

                    s"""
                           |auto $cName = [&](${member.zipWithIndex.map({ case (it,index) => s"${if isStruct.contains(it) then it+"*" else it} v$index"}).mkString(",")}) -> $name* {
                           |        return new $name{${constructors.map({ case (consName, _) => consName }).indexOf(cName)},{.$cName={ ${member.zipWithIndex.map("v"+_._2).mkString(",")} }}};
                           |    };
                           |""".stripMargin

                else
                    s"""
                       |$name* $cName = new $name{${constructors.map({ case (consName, _) => consName }).indexOf(cName)},{.$cName={}}};
                       |""".stripMargin
                    }).mkString
            }
                       |""".stripMargin
}

def getPatternCond(pattern: Pattern): String => String = {
    (obj: String) =>
        pattern match
            case Pattern.ConstructorDeconstruction(constructor, tupleItems) =>
                val index = getIndex(constructor)
                val buffer = ListBuffer.empty[String]
                var objName = s"$obj->v.$constructor"

                buffer.addOne(s"$obj->flag==$index")
                tupleItems.zipWithIndex.foreach{case (p,index) => buffer.addOne(getPatternCond(p)(s"$objName.v$index"))}
                buffer.mkString("&&")
            case Pattern.Identifier(ident) =>
                if zeroParaConstructors.contains(ident)
                    then
                        s"$obj == $ident"
                else "true"
            case Pattern.WildCard => "true"
}
def getPatternAssign(pattern: Pattern): String => String = {
    (obj: String) =>
        pattern match
            case Pattern.ConstructorDeconstruction(constructor, tupleItems) =>
                val buffer = ListBuffer.empty[String]
                var objName = s"$obj->v.$constructor"

                tupleItems.zipWithIndex.foreach{case (p,i) => buffer.addOne(getPatternAssign(p)(s"$objName.v$i"))}
                buffer.mkString
            case Pattern.Identifier(ident) =>
                if ! zeroParaConstructors.contains(ident) then
                s"auto $ident = $obj;\n"
                else ""
            case Pattern.WildCard => ""
}

def getIndex(cons:String): Int = {
    var res = -1;
    dataDefList.foreach({case Data(name,member) =>
        val index = member.indexWhere{case (n,_)=>n==cons}
        if index != -1 then res = index
    })
    res
}
