package frieren
/*
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
case class SymbolType(list: List[(Type, Boolean)]){
    var typeList: List[(Type, Boolean)] = list
    def polymorphic: Boolean = typeList.head._2
    def add(t:(Type, Boolean)): Unit = {
        typeList = typeList.::(t)
    }
    def update(t:Type): Unit = {
        typeList = typeList.tail.::((t, false))
    }
    def remove_isEmpty(): Boolean = {
        typeList = typeList.tail
        typeList.isEmpty
    }
    def value: Type = {
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
            update(typeList.head._1)
        }else{
            typeList.head._1
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
                typeMap(it).add((Type.Var(count), false))
            }else{
                typeMap += (it -> SymbolType(List((Type.Var(count), false))))
            }
        )
        val res = solveLambda(param, solveBlock(body))
        param.foreach(it =>
            if(typeMap(it).remove_isEmpty()){
                typeMap -= it
            }
        )
        res
    case Apply(func, arg) =>
        solveApplyList(inferNode(func), inferNodeList(arg))
    case variable: Symbol =>
        typeMap(variable).value
    case _:Number =>
        Type.RealType(RType.Int)
    case _:Bool =>
        Type.RealType(RType.Bool)
    case Add(value1, value2) =>
        val add = Type.Arrow(Type.RealType(RType.Int), Type.Arrow(Type.RealType(RType.Int), Type.RealType(RType.Int)))
        solveApplyList(add, inferNodeList(List(value1, value2)))
    case Mul(value1, value2) =>
        val mul = Type.Arrow(Type.RealType(RType.Int), Type.Arrow(Type.RealType(RType.Int), Type.RealType(RType.Int)))
        solveApplyList(mul, inferNodeList(List(value1, value2)))
    case Let(bindings, in) =>
        bindings.foreach(it =>
            val (symbol, astNode) = it
            val value = inferNode(astNode)
            if (typeMap.contains(symbol)) {
                typeMap(symbol).add((value, true))
            } else {
                typeMap += (symbol -> SymbolType(List((value, true))))
            }
        )
        val res = solveBlock(in)
        bindings.foreach(it =>
            val (symbol, astNode) = it
            if (typeMap(symbol).remove_isEmpty()) {
                typeMap -= symbol
            }
        )
        res
}

def solveBlock(list: List[AstNode]): Type = {
    var res:Type = RealType(RType.Unit)
    list.foreach(it =>
        res = inferNode(it)
    )
    res
}

def inferNodeList(list: List[AstNode]): List[Type] = {
    list.map(x => inferNode(x))
}

def solveLambda(list: List[Symbol], result:Type):Type = {
    var t1 = result
    val reverse = list.reverse
    reverse.foreach(it =>
        t1 = Type.Arrow(typeMap(it).value, t1)
    )
    t1
}

def solveApplyList(func: Type, arg: List[Type]): Type = {
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
                symbolType.update(updateInEnv(symbolType.value))
            }
        )
    }

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
                case a: Type.Arrow =>
                    checkSelfLoop(v, a)
                    env += (v -> a)
                case rv: Type.Var =>
                    if (v != rv) {
                        env += (v -> rv)
                    }
                case re: Type.RealType =>
                    env += (v -> re)
        case re: Type.RealType =>
            updateInEnv(r) match {
                case v: Type.Var =>
                    env += (v -> re)
                case re1: Type.RealType =>
                    if (re != re1) {
                        throw new WrongTypeException("WrongType")
                    }
                case arrow: Type.Arrow =>
                    throw new WrongTypeException("WrongType")

            }
    }

    def solveApply(l: Type, r: Type): Type = {
        l match {
            case Type.Arrow(left, right) =>
                solveEquation(left, r)
                updateSymbol()
                updateInEnv(right)
            case v: Type.Var =>
                count += 1
                val t1 = Type.Arrow(r, Type.Var(count))
                checkSelfLoop(v, t1)
                env += (v -> t1)
                updateSymbol()
                Type.Var(count)
            case re: Type.RealType =>
                throw new WrongTypeException("WrongType")
        }
    }

    var res = func
    arg.foreach(it =>
        res = solveApply(res, it)
    )
    res
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


*/