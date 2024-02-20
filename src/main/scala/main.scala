package frieren

import scala.annotation.tailrec

@main
def main(): Unit = {
  repl()
}

@tailrec
def repl(): Unit = {
  print("Frieren> ")

  val input = scala.io.StdIn.readLine()

  if (input.toLowerCase == ":q") {
    println("Goodbye!")
  } else {
    try {
      val ast = FrierenParser.parseToAst(input)
      //println(s"parsed to: $ast")
      println(s"type inferred to: \n${infer(ast)}")
      //println(s"interpreted to: ${interp(ast, Map[String, Value]())}")
      //println(s"compiled to: ${compile(ast)}")
    } catch {
      case e: Throwable =>
        println(s"Error: ${e.getMessage}")
    }

    repl()
  }
}