package jp.co.teruuu.bnf

import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec

import scala.io.Source

class ConvertSpec extends AnyFunSpec with Diagrams {
  describe("ConverterSpec") {
    val bnf_definition = Source.fromFile(getClass.getClassLoader.getResource("bnf/bnf_1.bnf").getFile).getLines().toList
    val question = Source.fromFile(getClass.getClassLoader.getResource("bnf/question_1.txt").getFile).getLines().toList
    val jsonkeys = Source.fromFile(getClass.getClassLoader.getResource("bnf/jsonkeys_1.txt").getFile).getLines().toList
    val ans = Source.fromFile(getClass.getClassLoader.getResource("bnf/ans_1.txt").getFile).getLines().toList.map(_.equals("true"))

    it("convert") {
      val convertResult = Converter.jsonConvert(bnf_definition, question, jsonkeys)
      assert(convertResult.length == ans.foldLeft(0)((acc, cur) => if(cur) {acc + 1} else {acc}))
    }
  }

  describe("ConverterSpec1") {
    val bnf_definition = List("<number> ::= (\"0\"|\"1\"|\"2\"|\"3\"|\"4\"|\"5\"|\"6\"|\"7\"|\"8\"|\"9\")",
      "<lower> ::= (\"a\"|\"b\"|\"c\"|\"d\"|\"e\"|\"f\"|\"g\"|\"h\"|\"i\"|\"j\"|\"k\"|\"l\"|\"m\"|\"n\"|\"o\"|\"p\"|\"q\"|\"r\"|\"s\"|\"t\"|\"u\"|\"v\"|\"w\"|\"x\"|\"y\"|\"z\")",
      "<upper> ::= (\"A\"|\"B\"|\"C\"|\"D\"|\"E\"|\"F\"|\"G\"|\"H\"|\"I\"|\"J\"|\"K\"|\"L\"|\"M\"|\"N\"|\"O\"|\"P\"|\"Q\"|\"R\"|\"S\"|\"T\"|\"U\"|\"V\"|\"W\"|\"X\"|\"Y\"|\"Z\")",
      "<comma> ::= \".\"",
      "<char> ::= (<number>|<lower>|<upper>|<comma>)",
      "<host> ::= <char> +",
      "<dir> ::= (<char>|\"/\") +",
      "<key> ::= (<lower>|<upper>)+",
      "<value> ::= (<lower>|<upper>|<number>)+",
      "<param> ::= <key> \"=\" <value>",
      "<params> ::= <param> (\"&\"<param>)*",
      "<scheme> ::= \"https\"|\"http\"",
      "<uri> ::= <scheme> \"://\" <host> [\"/\" <dir> [\"?\" <params>]]",
      "<root> ::= <uri>"
    )
    val inputs = List("https://github.com/user/project?abc=def&ghi=jkl")
    val json_keys = List("scheme: scheme", "host: host", "key: key", "value: value")
    Converter.jsonConvert(bnf_definition, inputs, json_keys).foreach {
      case (line, json) => {
        println(s"line number: ${line}")
        println(json)
      }
    }
  }
}
