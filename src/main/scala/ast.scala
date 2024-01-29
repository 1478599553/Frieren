package frieren

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
enum Value{
    case Number(value: Int)
    case Closure(lam:Abstraction, env: Env)
}