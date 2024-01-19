package frieren

import Type.RealType

import java.lang.invoke.WrongMethodTypeException

enum Type {
    case Arrow(left: Type, right:Type)
    case Var(num: Int)
    case RealType(name: RType)

    override def toString: String = {
        this match
            case Type.Arrow(l: Type.Arrow, r) =>
                s"(${l.toString})->${r.toString}"
            case Type.Arrow(l: Type, r) =>
                s"${l.toString}->${r.toString}"
            case Type.Var(num) =>
                s"t$num"
            case Type.RealType(rType) =>
                s"$rType"
    }
}

enum RType {
    case Int
    case Bool
    case Unit
    case WrongType(message: String)
}

var typeMap: Map[Symbol, SymbolType] = Map()
var count = 0
case class SymbolType(list: List[Type], poly: Boolean){
    var typeList: List[Type] = list
    var polymorphic: Boolean = poly
    def add(t:Type): Unit = {
        typeList = typeList.::(t)
    }
    def update(t:Type): Unit = {
        typeList = typeList.tail.::(t)
    }
    def remove_isEmpty(): Boolean = {
        typeList = typeList.tail
        typeList.isEmpty
    }
    def head: Type = {
        if(polymorphic){
            var env: Map[Type.Var, Type.Var] = Map()
            def update(t:Type):Type = t match{
                case v:Type.Var =>
                    if(env.contains(v)){
                        env(v)
                    }else{
                        count += 1
                        val res:Type.Var = Type.Var(count)
                        env += (v -> res)
                        res
                    }
                case Type.Arrow(l, r) =>
                    Type.Arrow(update(l), update(r))
                case re:Type.RealType =>
                    re
            }
            update(typeList.head)
        }else{
            typeList.head
        }
    }
}

class WrongTypeException(message: String) extends Exception(message)
def infer(input: AstNode): Type = {
    typeMap = Map()
    count = 0
    try {
        inferNode(input)
    }catch
        case w:WrongTypeException => Type.RealType(RType.WrongType(w.getMessage))

}

def inferNode(input: AstNode): Type = input match{
    case Abstraction(param, body) =>
        param.foreach(it =>
            count += 1
            if(typeMap.contains(it)){
                typeMap(it).add(Type.Var(count))
            }else{
                typeMap += (it -> SymbolType(List(Type.Var(count)), false))
            }
        )
        val res = solveLambda(param, solveNodeList(body))
        param.foreach(it =>
            if(typeMap(it).remove_isEmpty()){
                typeMap -= it
            }
        )
        res
    case Apply(func, arg) =>
        var res = inferNode(func)
        arg.foreach(it =>
            res = solveApply(res, inferNode(it))
        )
        res
    case variable: Symbol =>
        typeMap(variable).head
    case _:Number =>
        Type.RealType(RType.Int)
    case _:Bool =>
        Type.RealType(RType.Bool)
    case Add(value1, value2) =>
        val add = Type.Arrow(Type.RealType(RType.Int), Type.Arrow(Type.RealType(RType.Int), Type.RealType(RType.Int)))
        val res1 = solveApply(add, inferNode(value1))
        val res2 = solveApply(res1, inferNode(value2))
        res2
    case Let(bindings, in) =>
        bindings.foreach(it =>
            val (symbol, astNode) = it
            val value = inferNode(astNode)
            if (typeMap.contains(symbol)) {
                typeMap(symbol).add(Type.Var(count))
            } else {
                typeMap += (symbol -> SymbolType(List(value), true))
            }
        )
        val res = solveNodeList(in)
        bindings.foreach(it =>
            val (symbol, astNode) = it
            if (typeMap(symbol).remove_isEmpty()) {

            }
        )
        res
}

def solveNodeList(list: List[AstNode]): Type = {
    var res:Type = RealType(RType.Unit)
    list.foreach(it =>
        res = inferNode(it)
    )
    res
}

def solveLambda(list: List[Symbol], result:Type):Type = {
    var t1 = result
    val reverse = list.reverse
    reverse.foreach(it =>
        t1 = Type.Arrow(typeMap(it).head, t1)
    )
    t1
}

def solveApply(func: Type, arg: Type): Type = {
    var env: Map[Type.Var, Type] = Map()

    def updateInEnv(input: Type): Type = input match {
        case v: Type.Var =>
            if (env.contains(v)) {
                val next = updateInEnv(env(v))
                env += (v -> next)
                next
            } else {
                v
            }
        case Type.Arrow(left, right) =>
            Type.Arrow(updateInEnv(left), updateInEnv(right))
        case re:Type.RealType =>
            re
    }

    def updateSymbol():Unit = {
        typeMap.foreach((symbol, symbolType) =>
            if(!symbolType.polymorphic){
                symbolType.update(updateInEnv(symbolType.head))
            }
        )
    }

    func match {
        case Type.Arrow(left, right) =>
            solveEquation(left, arg)
            def solveEquation(l: Type, r: Type): Unit = updateInEnv(l) match {
                case Type.Arrow(ll, lr) =>
                    updateInEnv(r) match {
                        case Type.Arrow(rl, rr) =>
                            solveEquation(ll, rl)
                            solveEquation(lr, rr)
                        case v: Type.Var =>
                            env += (v -> l)
                        case re: Type.RealType =>
                            throw new WrongTypeException("WrongType")
                    }
                case v: Type.Var =>
                    updateInEnv(r) match
                        case a:Type.Arrow =>
                            checkSelfLoop(v, a)
                            env += (v -> a)
                        case rv:Type.Var =>
                            if(v != rv){
                                env += (v -> rv)
                            }
                        case re:Type.RealType =>
                            env += (v -> re)
                case re: Type.RealType =>
                    updateInEnv(r) match {
                        case v: Type.Var =>
                            env += (v -> re)
                        case re1:Type.RealType =>
                            if(re != re1){
                                throw new WrongTypeException("WrongType")
                            }
                        case arrow: Type.Arrow =>
                            throw new WrongTypeException("WrongType")

                    }
            }
            updateSymbol()
            updateInEnv(right)

        case v: Type.Var =>
            count += 1
            val t1 = Type.Arrow(arg, Type.Var(count))
            checkSelfLoop(v, t1)
            env += (v -> t1)
            updateSymbol()
            Type.Var(count)
        case re:Type.RealType =>
            throw new WrongTypeException("WrongType")
    }
}

def checkSelfLoop(left:Type.Var, right: Type):Unit = {
    right match{
        case v:Type.Var =>
            if(left == v){
                throw new WrongTypeException("SelfLoop")
            }
        case Type.Arrow(l, r) =>
            checkSelfLoop(left, l)
            checkSelfLoop(left, r)
        case _:Type.RealType =>
    }
}


