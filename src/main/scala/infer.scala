package frieren

import java.lang.invoke.WrongMethodTypeException

enum Type {
    case Arrow(left: Type, right:Type)
    case Var(num: Int)
    case RealType(name: String)

    override def toString: String = {
        this match
            case Type.Arrow(l: Type.Arrow, r) =>
                s"(${l.toString})->${r.toString}"
            case Type.Arrow(l: Type, r) =>
                s"${l.toString}->${r.toString}"
            case Type.Var(num) =>
                s"t$num"
            case Type.RealType(name) =>
                s"$name"
    }
}

var t:Map[Symbol, List[Type]] = Map()
var count = 0

class WrongTypeException(message: String) extends Exception(message)
def infer(input: AstNode): Type = {
    t = Map()
    count = 0
    try {
        inferNode(input)
    }catch
        case w:WrongTypeException => Type.RealType(w.getMessage)

}

def inferNode(input: AstNode): Type = input match{
    case Abstraction(param, body) =>
        param.foreach(it =>
            count += 1
            if(t.contains(it)){
                t += (it -> t(it).::(Type.Var(count)))
            }else{
                t += (it -> List(Type.Var(count)))
            }
        )
        val res = solveLambda(param, inferNode(body.head))
        param.foreach(it =>
            if(t(it).tail.isEmpty){
                t -= it
            }else{
                t += (it -> t(it).tail)
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
        t(variable).head
    case _:Number =>
        Type.RealType("Int")
    case Add(value1, value2) =>
        val add = Type.Arrow(Type.RealType("Int"), Type.Arrow(Type.RealType("Int"), Type.RealType("Int")))
        val res1 = solveApply(add, inferNode(value1))
        val res2 = solveApply(res1, inferNode(value2))
        res2
}

def solveLambda(list: List[Symbol], result:Type):Type = {
    var t1 = result
    val reverse = list.reverse
    reverse.foreach(it =>
        t1 = Type.Arrow(t(it).head, t1)
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
            t.foreach((symbol, tList) =>
                t += (symbol -> tList.tail.::(updateInEnv(tList.head)))
            )
            updateInEnv(right)

        case v: Type.Var =>
            count += 1
            val t1 = Type.Arrow(arg, Type.Var(count))
            checkSelfLoop(v, t1)
            env += (v -> t1)
            t.foreach((symbol, tList) =>
                t += (symbol -> tList.tail.::(updateInEnv(tList.head)))
            )
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


