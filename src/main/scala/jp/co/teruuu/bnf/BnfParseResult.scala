package jp.co.teruuu.bnf

sealed abstract class BnfParseResult {
}

case class ParsedStr(str: String) extends BnfParseResult {
  override def toString = s"""\"${str}\""""
}
case class ParsedSymbol(symbol: String, list: List[BnfParseResult]) extends BnfParseResult {
  override def toString = s"""{\"symbol\":\"${symbol}\", \"value\":[${list.map(_.toString).mkString(",")}]}"""
}

object BnfParseResult {

}
