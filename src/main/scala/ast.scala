package frieren

import Value.{BoolValue, ClosureValue, NumberValue}

sealed trait AstNode
case class Symbol(name: String) extends AstNode
case class Number(value: Int) extends AstNode
case class Add(value1: AstNode, value2: AstNode) extends AstNode
case class Mul(lhs: AstNode, rhs: AstNode) extends AstNode
case class Abstraction(param: List[Symbol], body: AstNode) extends AstNode
case class Apply(func: AstNode, arg: List[AstNode]) extends AstNode
case class Let(bindings : List[(Symbol, AstNode)], in : AstNode) extends AstNode
case class Bool(value: Boolean) extends AstNode
case class Block(content:List[AstNode]) extends AstNode
case class Match(obj: AstNode, arms: List[(Pattern,AstNode)]) extends AstNode
case class Data(name:String , constructors : List[(String,List[String])]) extends AstNode

enum Pattern{
    case ConstructorDestruction(constructor: String, tupleItems:List[String]) extends Pattern
    case Identifier(ident:String) extends Pattern
    case WildCard extends Pattern
}

enum Value{
    case NumberValue(value: Int)
    case ClosureValue(lam:Abstraction, env: Env)
    case BoolValue(bool: Boolean)
    private def operator(target: Value,operator: (Int,Int)=>Int): Option[Value] ={
        this match
            case NumberValue(value) => {
                target match
                    case NumberValue(value2) => Some(NumberValue(operator(value,value2)))
                    case Value.ClosureValue(_, _) => None
                    case Value.BoolValue(_) => None
            }
            case ClosureValue(_, _) => None
            case BoolValue(_) => None
    }
    def +(target:Value): Option[Value] = {
        operator(target,(_ + _))
    }

    def *(target: Value): Option[Value] = {
        operator(target,(_ * _)) 
    }
}
