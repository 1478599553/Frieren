package frieren

enum Type {
    case Arrow(left: Type, right:Type)
    case Var(variable: Symbol)

    override def toString: String = {
        this match
            case Type.Arrow(l: Type.Var, r) =>
                s"${l.toString}->${r.toString}"
            case Type.Arrow(l: Type.Arrow, r) =>
                s"(${l.toString})->${r.toString}"
            case Type.Var(variable) =>
                s"${variable.name}"
    }
}

var t:Map[Symbol, Type] = Map()

def infer(input: AstNode): Type = input match{
    case Abstraction(param, body) =>
        param.foreach(it =>
            t += (it -> Type.Var(it))
        )
        solveLambda(param, body)
    case Apply(func, arg) =>
        val t1 = infer(func)
        t1 match
            case Type.Var(variable) =>
                val t2 = updateVar(t(variable), solveList(arg, Type.Var(variable)))
                t += (variable -> t2)
                t1
            case arrow: Type.Arrow =>
                var a: Type = arrow
                arg.foreach(it =>
                    a match
                        case a1:Type.Arrow =>
                            a = solveApply(a1, infer(it))
                        case _:Type.Var =>
                            a
                )
                a
    case variable: Symbol =>
        t(variable)
    case Number(value) =>
        Type.Var(Symbol("Int"))     //todo
    case Add(value1, value2) =>
        infer(value1)   //todo
}

def solveLambda(list: List[Symbol], body:List[AstNode]):Type = {
    var t1 = infer(body.head)
    val reverse = list.reverse
    reverse.foreach(it =>
        t1 = Type.Arrow(t(it), t1)
    )
    t1
}

def solveList(list: List[AstNode], end: Type): Type = {
    if(list.isEmpty){
        end
    }else{
        Type.Arrow(infer(list.head), solveList(list.tail, end))
    }
}
def solveApply(left: Type.Arrow, right: Type): Type = {
    var env:Map[Symbol, Type] = Map()
    def solveInEnv(l1: Type, r1: Type):Unit = l1 match{
        case Type.Var(variable) =>
            env += (variable -> r1)
        case Type.Arrow(ll, lr) =>
            r1 match {
                case v: Type.Var =>
                    env.foreach((variable, t1) => {
                        if (t1 == v) {
                            env += (variable -> l1)
                        }
                    })
                case Type.Arrow(rl, rr) =>
                    solveInEnv(ll, rl)
                    solveInEnv(lr, rr)
            }
    }
    solveInEnv(left.left, right)
    update(left.right, env)
}

def updateVar(left: Type, right: Type): Type = left match {
    case Type.Var(_) =>
        right
    case Type.Arrow(l, r) =>
        Type.Arrow(l, updateVar(r, right))
}

def update(input: Type, env: Map[Symbol, Type]): Type = input match{
    case Type.Var(variable) =>
        if(env.contains(variable)){
            env(variable)
        }else{
            Type.Var(variable)
        }
    case Type.Arrow(left, right) =>
        Type.Arrow(update(left, env), update(right, env))
}

