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
/*
def compile (expr:AstNode): String = {
    expr match
        case Symbol(name) => s"lambda env : env[\"$name\"]"
        case Number(value) => s"lambda env : $value"
        case Add(value1, value2) =>
            val lhs = compile(value1)
            val rhs = compile(value2)
            s"lambda env : ($lhs)(env) + ($rhs)(env)"
        case Mul(lhs, rhs) =>
            val lhsV = compile(lhs)
            val rhsV = compile(rhs)
            s"lambda env : ($lhsV)(env) * ($rhsV)(env)"
        case Abstraction(param, body) =>
            val bodyV = compile(body)
            val paraList = param.mkString(",")
            s"lambda env : (lambda $paraList : $bodyV(env))"
        case Apply(func, arg) =>
            val funcV = compile(func)
            val argList = arg.mkString(",")
            s"lambda env : ($funcV)($argList)"
        case Let(bindings, in) => ???
        case Bool(value) => ???
        case Block(content) => ???
}
*/
def compile(expr:AstNode) : String = {
    expr match
        case Symbol(name) => name
        case Number(value) => value.toString
        case Add(value1, value2) => s"${compile(value1)}+${compile(value2)}"
        case Mul(lhs, rhs) => s"${compile(lhs)}*${compile(rhs)}"
        case Abstraction(param, body) => s"[=](${param.map({ case Symbol(it) => it }).map(it => s"auto $it").mkString(",")}) {return ${compile(body)};}"
        case Apply(func, arg) => s"(${compile(func)})(${arg.map(compile).mkString(",")})"
        case Let(bindings, in) =>
            s"([=](){" +
            s"${bindings.map({case (Symbol(name),v) => s"auto $name = ${compile(v)};\n"}).mkString("")}" +
                s"return ${compile(in)};"+
            s"})()"
        case Bool(value) => value.toString
        case Block(content) => ???
}