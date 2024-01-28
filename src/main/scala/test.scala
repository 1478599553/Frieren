package frieren


private val caseList = List(

    """
      |1+(2+3*4+(5+6)*7*(8+9)) * 10 + 11
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