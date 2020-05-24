package com.github.kmizu.scomb

import org.scalatest.{DiagrammedAssertions, FunSpec}

class MySpec extends FunSpec with DiagrammedAssertions {
  import com.github.kmizu.scomb._

  object P2 extends SCombinator {

    def parseNum: P[Int] = set('0' to '9').+ ^^ { case digit:List[String] => digit.mkString.toInt}
    def root: P[String] = $("abc")
  }



  describe("test defaultToken") {
    println(P2.parsePartial(P2.root, "abc def"))

    println(P2.parsePartial(P2.parseNum, "123 abc"))
  }


}
