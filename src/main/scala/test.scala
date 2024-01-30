package frieren

/*
private val caseList = List(
    "((lambda x (x 1 2)) (lambda x x))",
    "(lambda x (lambda f (lambda g (g (f x) (f x x)))))",
    "(lambda x ((lambda (f x) (f x)) x))",
    "(lambda (f x a) (f x (x a)))",
    "(lambda (a b c d e) ((lambda (a b f g h) (a b c d e f g h))))",
    "(lambda x (+ x 1))",
    "((lambda x (x x)) (lambda x (x x)))",
    "(+ 1 ((lambda x (+ x 2)) 3))",
    "((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x))",
    "((lambda f (f (lambda (x y) x) (lambda (x y) x))) (lambda (x y) ((x (lambda x x) 1) (y 2 3))))",
    "(lambda (f g x) (f x (g x)))",
    "((lambda (f g x) (f x (g x))) ((lambda (x y) x) ((lambda (f g x) (f x (g x))) ((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x)))) (lambda (x y) x))",
    "((lambda (s1 k1 s2 i k2) (s1 (k1 (s2 i)) k2)) (lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (f g x) (f x (g x))) ((lambda (f g x) (f x (g x))) (lambda (x y) x) (lambda (x y) x)) (lambda (x y) x))",
    "(lambda x (lambda y y))",
    "(lambda a (lambda b (lambda f (f (a b)))))", //λ a. λ b. λ f. f a b
    "(lambda x1 (x1 x1))"

)
*/
private val caseList = List(

    """
      |
      | let (f = fn a => a + a * 5,b=2) in
      | f(b)
      |
      |
      |""".stripMargin

)


object test extends App {
    testParser()
    testInfer()
    testInterp()
}

def testParser(): Unit = {

    caseList.foreach(it =>
        println(s"$it => ${FrierenParser.parseToAst(it)}")
    )

}
def testInfer(): Unit = {
    caseList.foreach(it =>
        println(s"${infer(FrierenParser.parseToAst(it))}")
    )
}

def testInterp(): Unit = {
    caseList.foreach(it=>
        println(s"$it => ${interp(FrierenParser.parseToAst(it),Map[Symbol,Value]())}")
    )
}