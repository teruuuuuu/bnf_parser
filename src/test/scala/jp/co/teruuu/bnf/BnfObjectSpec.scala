package jp.co.teruuu.bnf

import com.github.kmizu.scomb.Result
import org.scalatest.diagrams.Diagrams
import org.scalatest.funspec.AnyFunSpec
import jp.co.teruuu.bnf.BnfObject.BnfParser

import scala.io.Source

class BnfObjectSpec extends AnyFunSpec with Diagrams {
  describe("BnfObjectSpec1") {
    val bnf_definition = Source.fromFile(getClass.getClassLoader.getResource("bnf/bnf_1.bnf").getFile).getLines().toList
    val question = Source.fromFile(getClass.getClassLoader.getResource("bnf/question_1.txt").getFile).getLines().toList
    val ans = Source.fromFile(getClass.getClassLoader.getResource("bnf/ans_1.txt").getFile).getLines().toList.map(_.equals("true"))
    val (load_result, bnf_opt) = BnfParser.apply(bnf_definition)

    it("load bnf definition") {
      assert(!load_result.exists(_.isInstanceOf[Result.Failure]))
    }

    it("parse by bnf") {
      assert(bnf_opt.isDefined)
      bnf_opt match {
        case Some(parser) => {
          question.zip(ans).foreach {
            case (q, a) => {
              assert(parser.parse(q).isInstanceOf[Result.Success[_]] == a)
            }
          }
        }
        case _ => assert(false)
      }
    }
  }

  describe("BnfObjectSpec2") {
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
    BnfParser.apply(bnf_definition) match {
      case (_, Some(parser)) => {
        println(s"=> ${parser.parse("https://github.com/user/project?abc=def&ghi=jkl").isInstanceOf[Result.Success[_]]}")
      }
    }
  }

}
