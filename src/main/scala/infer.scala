package frieren

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
    case Data(name: String)
    case WrongType(message: String)
}

var lasttypeMap: Map[Symbol, TypeList] = Map()
var lastdataMap: Map[String, Map[String, TypeList]] = Map()
var lastenv: Map[Type.Var, Type] = Map()
var lastcount = 0

var typeMap: Map[Symbol, TypeList] = Map()
var dataMap: Map[String, Map[String, TypeList]] = Map()
var env: Map[Type.Var, Type] = Map()
var cnt = 0
class TypeList(list: List[(Type, Boolean)]){
    private var typeList: List[(Type, Boolean)] = list
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
                        cnt += 1
                        val res:Type.Var = Type.Var(cnt)
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

def infer(input: AstNode): TypedAstNode = {
    typeMap = lasttypeMap
    env = lastenv
    cnt = lastcount
    try {
        val res = inferToTypedAst(input)
        lasttypeMap = typeMap
        lastenv = env
        lastcount = cnt
        res
    } catch
        case w: WrongTypeException => throw w

}

trait TypedAstNode{def typ: Type}
case class TypedSymbol(name: String, typ: Type) extends TypedAstNode
case class TypedNumber(value: Int, typ: Type) extends TypedAstNode
case class TypedAdd(value1: TypedAstNode, value2: TypedAstNode, typ: Type) extends TypedAstNode
case class TypedMul(lhs: TypedAstNode, rhs: TypedAstNode, typ: Type) extends TypedAstNode
case class TypedAbstraction(param: List[TypedSymbol], body: TypedAstNode, typ: Type) extends TypedAstNode
case class TypedApply(func: TypedAstNode, arg: List[TypedAstNode], typ: Type) extends TypedAstNode
case class TypedLet(bindings : List[(TypedSymbol, TypedAstNode)], in : TypedAstNode, typ: Type) extends TypedAstNode
case class TypedBool(value: Boolean, typ: Type) extends TypedAstNode
case class TypedBlock(content:List[TypedAstNode], typ: Type) extends TypedAstNode
case class TypedMatch(obj: TypedAstNode, arms: List[(Pattern,TypedAstNode)], typ: Type) extends TypedAstNode
case class TypedData(name:String , constructors : List[(String,List[String])], typ: Type) extends TypedAstNode

def inferToTypedAst(input: AstNode): TypedAstNode = input match {
    case Abstraction(param, body) =>
        param.foreach(it =>
            cnt += 1
            if (typeMap.contains(it)) {
                typeMap(it).add((Type.Var(cnt), false))
            } else {
                typeMap += (it -> TypeList(List((Type.Var(cnt), false))))
            }
        )
        val inferredBody: TypedAstNode = inferToTypedAst(body)
        val res = solveLambda(param, inferredBody.typ)
        param.foreach(it =>
            if (typeMap(it).remove_isEmpty()) {
                typeMap -= it
            }
        )
        TypedAbstraction(res._1, inferredBody, res._2)
    case Apply(func, arg) =>
        val f = inferToTypedAst(func)
        val a = inferToTypedList(arg)
        TypedApply(f, a, solveApplyList(f.typ, a.map(_.typ)))
    case variable: Symbol =>
        if(!typeMap.contains(variable)){
            throw new WrongTypeException(s"${variable.name} is not defined")
        }
        TypedSymbol(variable.name, typeMap(variable).value)
    case number: Number =>
        TypedNumber(number.value, Type.RealType(RType.Int))
    case bool: Bool =>
        TypedBool(bool.value, Type.RealType(RType.Bool))
    case Add(value1, value2) =>
        val v1: TypedAstNode = inferToTypedAst(value1)
        val v2: TypedAstNode = inferToTypedAst(value2)
        val add = Type.Arrow(Type.RealType(RType.Int), Type.Arrow(Type.RealType(RType.Int), Type.RealType(RType.Int)))
        TypedAdd(v1, v2, solveApplyList(add, List(v1.typ, v2.typ)))
    case Mul(value1, value2) =>
        val v1: TypedAstNode = inferToTypedAst(value1)
        val v2: TypedAstNode = inferToTypedAst(value2)
        val mul = Type.Arrow(Type.RealType(RType.Int), Type.Arrow(Type.RealType(RType.Int), Type.RealType(RType.Int)))
        TypedMul(v1, v2, solveApplyList(mul, List(v1.typ, v2.typ)))
    case Let(bindings, in) =>
        var typedBindings: List[(TypedSymbol, TypedAstNode)] = Nil
        bindings.foreach((symbol, astNode) =>
            val typed: TypedAstNode = inferToTypedAst(astNode)
            typedBindings = (TypedSymbol(symbol.name, typed.typ), typed)::typedBindings
            if (typeMap.contains(symbol)) {
                typeMap(symbol).add((typed.typ, true))
            } else {
                typeMap += (symbol -> TypeList(List((typed.typ, true))))
            }
        )
        val res = inferToTypedAst(in)
        bindings.foreach(it =>
            val (symbol, astNode) = it
            if (typeMap(symbol).remove_isEmpty()) {
                typeMap -= symbol
            }
        )
        TypedLet(typedBindings.reverse, res, res.typ)
    case Block(content) =>
        var typedContent: List[TypedAstNode] = Nil
        content.foreach(it =>
            typedContent = inferToTypedAst(it)::typedContent
        )
        TypedBlock(typedContent.reverse, typedContent.head.typ)
    case Match(obj, arms) =>
        ???
    case Data(name, constructors) =>
        val typ = Type.RealType(RType.Data(name))
        def cons(list: List[String], end: Type): Type = list match{
            case ::(head, next) =>
                val h: Type = head match
                    case "Int" => Type.RealType(RType.Int)
                    case "Bool" => Type.RealType(RType.Bool)
                    case "Unit" => Type.RealType(RType.Unit)
                    case data => Type.RealType(RType.Data(data))
                Type.Arrow(h, cons(next, end))
            case Nil => end
        }
        constructors.foreach((name1, list) =>{
            val symbol = Symbol(name1)
            val typ1: Type = cons(list, typ)
            if (typeMap.contains(symbol)) {
                typeMap(symbol).add((typ1, true))
            } else {
                typeMap += (symbol -> TypeList(List((typ1, true))))
            }
        })
        TypedData(name, constructors, typ)
}

def inferToTypedList(list: List[AstNode]): List[TypedAstNode] = {
    list.map(x => inferToTypedAst(x))
}

def solveLambda(list: List[Symbol], body: Type):(List[TypedSymbol], Type) = {
    var t1:(List[TypedSymbol], Type) = (Nil, body)
    val reverse = list.reverse
    reverse.foreach(it =>
        t1 = (TypedSymbol(it.name, typeMap(it).value)::t1._1, Type.Arrow(typeMap(it).value, t1._2))
    )
    t1
}

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
    case re: Type.RealType =>
        re
}

def solveApplyList(func: Type, arg: List[Type]): Type = {


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
                    checkSelfLoop(v, Type.Arrow(ll, lr))
                    env += (v -> l)
                case re: Type.RealType =>
                    throw new WrongTypeException(s"$re can't apply")
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
                        throw new WrongTypeException(s"Require $re, but found $re1")
                    }
                case arrow: Type.Arrow =>
                    throw new WrongTypeException(s"Require $re, but found $arrow")

            }
    }

    def solveApply(l: Type, r: Type): Type = {
        updateInEnv(l) match {
            case Type.Arrow(left, right) =>
                solveEquation(left, r)
                updateSymbol()
                updateInEnv(right)
            case v: Type.Var =>
                cnt += 1
                val t1 = Type.Arrow(r, Type.Var(cnt))
                checkSelfLoop(v, t1)
                env += (v -> t1)
                updateSymbol()
                Type.Var(cnt)
            case re: Type.RealType =>
                throw new WrongTypeException(s"$re can't apply")
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

