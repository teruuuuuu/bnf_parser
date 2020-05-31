package jp.co.teruuu.converter

import jp.co.teruuu.converter.bnf.BnfParser
import spray.json.DefaultJsonProtocol

import scala.io.Source

object BnfConverter extends DefaultJsonProtocol {

  def main(args: Array[String]): Unit = {
    kakunin
    val bnf_file = Source.fromFile("sample.bnf").getLines().toList
    val log_file = Source.fromFile("sample.log").getLines().toList
    show(bnf_file, log_file)
  }

  def show(bnf_def: List[String], inputs: List[String]): Unit = {
    println(s"load bnf: ${bnf_def.mkString("\n")}")
    println(s"load bnf result: ${BnfParser.load_bnf(bnf_def)}")
    inputs foreach { input =>
      println(s"parse: ${input} result: ${BnfParser.parse(input)}")
    }
    println
  }

  private def kakunin: Unit = {
    show(List("root ::= \"Hello\" "),
      List("", "Hello", "HelloHello"))

    show(List("root ::= \"Hello\" +"),
      List("", "Hello", "HelloHello"))

    show(List("root ::= \"Hello\" *"),
      List("", "Hello", "HelloHello"))

    show(List("root ::= ( \"Hello\" \"World\") *"),
      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"))

    show(List("root ::= ( \"Hello\" [\"World\"]) *"),
      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"))

    show(List("root ::= { \"Hello\" [\"World\"] }"),
      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"))

    show(List("root ::= (\"Hello\" \"World\") | (\"こんにちは\" \"世界\")"),
      List("", "Hello", "HelloWorld", "こんにちは", "こんにちは世界"))

    show(
      List(
        "trimed ::= {notsp sp} notsp",
        "root ::= trimed" ),
      List(
        "", "こんにちは世界", "こんにちは 世界", "こ ん に ち は 世 界 !",
        "Hello", "Hello World", "H e l l o W o r l d", " not trimed "))
  }
}
