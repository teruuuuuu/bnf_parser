package jp.co.teruuu.converter

import com.github.kmizu.scomb.Result
import jp.co.teruuu.converter.bnf.BnfParser
import spray.json.DefaultJsonProtocol

import scala.io.Source

object BnfConverter extends DefaultJsonProtocol {

  def main(args: Array[String]): Unit = {
    val bnf_file = Source.fromFile("sample.bnf").getLines().toList
    val log_file = Source.fromFile("sample.log").getLines().toList
    val jsonKeys = Source.fromFile("json.keys").getLines().toList
      .foldLeft(Map.empty[String, List[String]])((acc,cur) =>
        cur.split(":").toList match {
          case a :: b => acc updated(a.trim, b.head.split(",").toList.map(_.trim))
        }
      )
    show(bnf_file, log_file, jsonKeys)
  }

  def show(bnf_def: List[String], inputs: List[String], jsonKeys: Map[String, List[String]]): Unit = {
    println(s"load bnf: ${bnf_def.mkString("\n")}")
    println(s"load bnf result: ${BnfParser.load_bnf(bnf_def)}")


    inputs foreach { input =>
      BnfParser.parse(input) match {
        case a: Result.Success[BnfParser.ParsedSymbol] =>
          println("parse success:" + BnfParser.ParsedSymbol.toJson(a.semanticValue, jsonKeys))
        case _ =>
          println("parse failed:" + input)
      }
    }
  }

//  private def kakunin: Unit = {
//    val jsonKeys = Map("root" -> List("root"))
//
//    show(List("root ::= \"Hello\" "),
//      List("", "Hello", "HelloHello"), jsonKeys)
//
//    show(List("root ::= \"Hello\" +"),
//      List("", "Hello", "HelloHello"), jsonKeys)
//
//    show(List("root ::= \"Hello\" *"),
//      List("", "Hello", "HelloHello"), jsonKeys)
//
//    show(List("root ::= ( \"Hello\" \"World\") *"),
//      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"), jsonKeys)
//
//    show(List("root ::= ( \"Hello\" [\"World\"]) *"),
//      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"), jsonKeys)
//
//    show(List("root ::= { \"Hello\" [\"World\"] }"),
//      List("", "Hello", "HelloWorld", "HelloWorldHelloWorld"), jsonKeys)
//
//    show(List("root ::= (\"Hello\" \"World\") | (\"こんにちは\" \"世界\")"),
//      List("", "Hello", "HelloWorld", "こんにちは", "こんにちは世界"), jsonKeys)
//
//    show(
//      List(
//        "trimed ::= {notsp sp} notsp",
//        "root ::= trimed" ),
//      List(
//        "", "こんにちは世界", "こんにちは 世界", "こ ん に ち は 世 界 !",
//        "Hello", "Hello World", "H e l l o W o r l d", " not trimed "), jsonKeys)
//  }
}
