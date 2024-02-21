package frieren

import java.awt.Toolkit
import java.awt.datatransfer.StringSelection
import scala.annotation.tailrec

@main
def main(): Unit = {
  repl()
}

implicit class PipeOperator[A](val value: A) extends AnyVal {
  def |>[B](f: A => B): B = f(value)
}
val cpb = Toolkit.getDefaultToolkit.getSystemClipboard
def clipboard(string: String): Unit = {
  val s = StringSelection(string)
  cpb.setContents(s,null)
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
      println(s"type inferred to: \n${prettyPrint(infer(ast))}")
      //println(s"interpreted to: ${interp(ast, Map[String, Value]())}")
      //println(s"compiled to: ${compile(ast)}")
    } catch {
      case e: Throwable =>
        println(s"Error: ${e.getMessage}")
    }

    repl()
  }
}