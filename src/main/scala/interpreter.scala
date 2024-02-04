package frieren

import Value.{BoolValue, ClosureValue, NumberValue}

type Env = Map[String,Value]
def interp (expr:AstNode,env:Env): Value = {
    expr match
        case Symbol(name) => env(name)
        case Number(value) => NumberValue(value)
        case Add(value1, value2) => (interp(value1,env)+interp(value2,env)).get
        case Mul(lhs, rhs) => (interp(lhs,env)*interp(rhs,env)).get
        case lam @ Abstraction(_, _) => ClosureValue(lam,env)
        
        case Apply(func, arg) => 
            val fun = interp(func,env)
            fun match
                case NumberValue(value) => throw Exception("Number cannot be applied to!")
                case ClosureValue(lam, envi) => 
                    if lam.param.length != arg.length then throw Exception("Wrong argument count in applying on function "+lam)
                    else {
                        val newEnv = lam.param.zip(arg.map(interp(_,env))).foldLeft(envi){
                            case (envir,(k,v)) => envir+(k.name->v)
                        }
                        interp(lam.body,newEnv)
                    }
                
        case Let(bindings, in) =>
            val newEnv = bindings.map({case (s,e) => (s,interp(e,env))}).foldLeft(env){
                case (envi,(k,v)) => envi+(k.name->v)
            }
            interp(in,newEnv)
        case Bool(value) => BoolValue(value)
        case Block(content) =>
            content match
                case ::(current,Nil) => interp(current,env)
                case ::(head, next) => interp(head,env);interp(Block(next),env)
}