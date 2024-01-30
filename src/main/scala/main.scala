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
      println(s"$input => ${FrierenParser.parseToAst(input)}")
      println(s"${infer(FrierenParser.parseToAst(input))}")
      println(s"$input => ${interp(FrierenParser.parseToAst(input), Map[Symbol, Value]())}")

    } catch {
      case e: Throwable =>
        println(s"Error: ${e.getMessage}")
    }

    repl()
  }
}