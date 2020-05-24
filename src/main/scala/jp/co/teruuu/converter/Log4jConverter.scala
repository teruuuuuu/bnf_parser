package jp.co.teruuu.converter

import java.time.LocalDateTime

import spray.json._
import spray.json.DefaultJsonProtocol
import jp.co.teruuu.converter.logging.{Log4jLine, LogLevel}

import scala.io.Source

object Log4jConverter extends DefaultJsonProtocol {
  import logging.Log4jParser

  implicit object ColorJsonFormat extends RootJsonFormat[Log4jLine] {
    def write(l: Log4jLine) = JsObject(
      "timestamp" -> JsString(l.date.toString),
      "level" -> JsString(l.logLevel.name),
      "package" -> JsString(l.name),
      "thread" -> JsString(l.thread),
      "content" -> JsString(l.content),
    )
    override def read(json: JsValue): Log4jLine =
      Log4jLine(LocalDateTime.now, LogLevel(""), "", "", "")
  }

  val source = """{ "some": "JSON source" }"""
  val jsonAst = source.parseJson // or JsonParser(source)

  def main(args: Array[String]): Unit = {
    var filePath = Source.fromFile(args(0))
    filePath.getLines.foreach(parseToJsonString(_).map(println))
  }

  def parseToJsonString(str: String): Option[String] = {
    Log4jParser.parse(str).value.map(_.toJson.toString())
  }
}
