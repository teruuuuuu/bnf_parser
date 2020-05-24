package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class ScombSpec extends FunSpec with DiagrammedAssertions {

  def tes(a: (String, Int)*) = {
    a.iterator.foreach(println)
    println(a)
  }

  val k = Array(("1", 1), ("2", 2), ("3", 3))
  val ite = k.iterator




  object JsonParser extends SCombinator {
    def escape(ch: Char): Char = ch match {
      case ' ' => ' '
      case 't' => '\t'
      case 'f' => '\f'
      case 'b' => '\b'
      case 'r' => '\r'
      case 'n' => '\n'
      case '\\' => '\\'
      case '"' => '"'
      case '\'' => '\''
      case otherwise => otherwise
    }

    lazy val string: Parser[String] = rule{for {
      _ <- DefaultSpace.*
      _ <- $("\"")
      contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString} | except('"')).*
      _ <- $("\"").l("double quote")
      _ <- DefaultSpace.*
    } yield contents.mkString}

//    lazy val jstring: Parser[String] = rule(string ^^ {v => v})

    def parse(input: String): Result[String] = parse(string, input)
  }


  import JsonParser._

  describe("test scomb") {
    it("aaa") {
      assert(Some("aaa") == parse("   \"aaa\"   ").value)
    }
  }

}
