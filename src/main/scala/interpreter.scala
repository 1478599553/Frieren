package frieren

type Env = Map[Symbol,Value]
def interp (expr:List[AstNode],env:Env): Value = {
    def interpSingle(expr:AstNode,env:Env) : Value = {
        expr match
            case Symbol(name) => ???
            case Number(value) => ???
            case Add(value1, value2) => ???
            case Mul(lhs, rhs) => ???
            case Abstraction(param, body) => ???
            case Apply(func, arg) => ???
            case Let(bindings, in) => ???
            case Bool(value) => ???
    }

    expr match
        case ::(current, Nil) => interpSingle(current, env)
        case ::(current, next) =>
            interpSingle(current,env)
            interp(next,env)
}
