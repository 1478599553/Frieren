package frieren


private val caseList = List(

    """
      |1+2
      |
      |
      |
      |
      |""".stripMargin

)


object test extends App {
    testParser()
    testInfer()
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