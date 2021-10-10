package jp.co.teruuu.bnf

import com.github.kmizu.scomb.Result
//import jp.co.teruuu.bnf.BnfObject.{BnfParser, ParsedSymbol}
//
//import scala.io.Source

trait ConvertOut[A] {
  def write(parseResult: Result[ParsedSymbol]): A
}
//
//case class JsonConvert(jsonKeys: Map[String, List[String]]) extends ConvertOut[Option[String]] {
//  override def write(parseResult: Result[ParsedSymbol]): Option[String] = {
//    parseResult match {
//      case a: Result.Success[ParsedSymbol] =>
//        Some(ParsedSymbol.toJson(a.semanticValue, jsonKeys))
//      case _ => None
//    }
//  }
//}
//
//object Converter {
//
//  def main(args: Array[String]): Unit = {
//    if(args.length < 2) {
//      println("prease input bnfDefFile parsedTargetFile jsonKeyFile")
//    }
//    val bnf_file = Source.fromFile(args(0)).getLines().toList
//    val log_file = Source.fromFile(args(1)).getLines().toList
//    val jsonKeys = loadJsonKeys(Source.fromFile(args(2)).getLines().toList)
//
//    jsonConvert(bnf_file, log_file, jsonKeys).foreach(println)
//  }
//
//  def loadJsonKeys(keys: List[String]): Map[String, List[String]] =
//    keys.foldLeft(Map.empty[String, List[String]])((acc,cur) =>
//      cur.split(":").toList match {
//        case a :: b => acc updated(a.trim, b.head.split(",").toList.map(_.trim))
//      })
//
//  def jsonConvert(bnf_def: List[String], inputs: List[String], jsonKeys: List[String]): List[(Int, String)] =
//    jsonConvert(bnf_def, inputs, loadJsonKeys(jsonKeys))
//  def jsonConvert(bnf_def: List[String], inputs: List[String], jsonKeys: Map[String, List[String]]): List[(Int, String)] = {
//    convert(bnf_def, inputs)(JsonConvert(jsonKeys)).filter(_._2.isDefined).map{
//      case (index, Some(json)) => (index, json)
//    }
//  }
//
//  def convert[A](bnf_def: List[String], inputs: List[String])(implicit c: ConvertOut[A]): List[(Int, A)] = {
//    BnfParser.apply(bnf_def) match {
//      case (_, Some(f)) =>
//        inputs.zipWithIndex.map {
//          case (input, index) => (index, c.write(f.parse(input)))
//        }
//    }
//  }
//}
