package jp.co.teruuu.bnf

import com.github.kmizu.scomb.{Location, Result, SCombinator}

object BnfParser extends SCombinator {
  def apply(definitions: List[String]): Result[BnfParser] = BnfParser().load_bnf(definitions)
}

case class BnfParser() extends SCombinator {
  var defMap = Map.empty[String, Parser[BnfParseResult]]

  def initial_symbol: Map[String, Parser[BnfParseResult]] = Map(
    "sp" -> rule(DefaultSpace.+ ^^ (a => ParsedSymbol("sp", List(ParsedStr(a.mkString))))),
    "notsp" -> rule(except(' ').+ ^^ (a => ParsedSymbol("notsp", List(ParsedStr(a.mkString))))),
  )

  def bnf_parser: Parser[Parser[BnfParseResult]] = for {
    _ <- DefaultSpace.*
    symbol <- symbol
    _ <- DefaultSpace.*
    _ <- $("::=")
    _ <- DefaultSpace.*
    definition <- definition
    _ <- DefaultSpace.*
  } yield definition ^^ {
    case (a: ParsedStr) => ParsedSymbol(symbol, List(a))
    case (a: ParsedSymbol) => ParsedSymbol(symbol, a.list)
  }

  lazy val symbol: Parser[String] = rule(
    for {
      _ <- $("<")
      a <- char
      b <- str
      _ <- $(">")
    } yield a + b
  )

  lazy val definition: Parser[Parser[BnfParseResult]] = rule(
    for {
      _ <- DefaultSpace.*
      a <- rule(string | optional | repeat | group | defined_symbol)
      _ <- DefaultSpace.*
      b <- rule($("+") | $("*")).?
      _ <- DefaultSpace.*
      c <- {
        rule($("|") ~ DefaultSpace.* ~ definition) ^^ { case _ ~ _ ~ c => c }
      }.*
      _ <- DefaultSpace.*
    } yield rule(b match {
      case None => c.foldLeft(a)((acc, cur) => (acc | cur))
      case Some("+") => c.foldLeft(a.+ ^^ (x => ParsedSymbol("_", x).asInstanceOf[BnfParseResult]))((acc, cur) => acc | cur)
      case Some("*") => c.foldLeft(a.* ^^ (x => ParsedSymbol("_", x).asInstanceOf[BnfParseResult]))((acc, cur) => acc | cur)
      case _ => sys.error("")
    })
  ).+ ^^ (a => a.tail.foldLeft(a.head)((acc, cur) =>
    acc ~ cur ^^ {
      case ((a: BnfParseResult) ~ (b: BnfParseResult)) => ParsedSymbol("_", List(a) ::: List(b))
    }))

  lazy val char: Parser[String] = rule(
    for {
      a <- (set('a' to 'z') | set('A' to 'Z'))
    } yield a
  )

  lazy val str: Parser[String] = rule(
    for {
      a <- (char | set('0' to '9')).* ^^ (_.mkString)
    } yield a
  )

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

  lazy val string: Parser[Parser[BnfParseResult]] = rule(
    for {
      _ <- $("\"")
      contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString } | except('"')).*
      _ <- $("\"").l("double quote")
    } yield $(contents.mkString) ^^
      ParsedStr
  )

  lazy val optional: Parser[Parser[BnfParseResult]] = rule(
    for {
      _ <- $("[")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $("]")
    } yield a.? ^^ {
      case Some(x) => x
      case _ => ParsedSymbol("_", List())
    }
  )

  lazy val group: Parser[Parser[BnfParseResult]] = rule(
    for {
      _ <- $("(")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $(")")
    } yield a
  )

  lazy val repeat: Parser[Parser[BnfParseResult]] = rule(
    for {
      _ <- $("{")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $("}")
    } yield a.* ^^ (as => ParsedSymbol("_", as))
  )

  lazy val defined_symbol: Parser[Parser[BnfParseResult]] =
    symbol ^^ (symbol => defMap.getOrElse(symbol, sys.error("symbol not defined")))

  private def load_bnf(definitions: List[String]): Result[BnfParser] = {

    defMap = initial_symbol
    definitions.foreach(definition => {
      parse(symbol, definition.split("::=")(0).trim) match {
        // 先に定義済みのシンボルを保存
        case Result.Success(symbolStr) => defMap = defMap updated(symbolStr, $("") ^^ (_ => ParsedStr("")))
      }
    })
    definitions.map(definition => parse(bnf_parser, definition) match {
      case Result.Success(a) => {
        parse(symbol, definition.split("::=")(0).trim) match {
          // 定義済みのシンボルを更新
          case Result.Success(symbolStr) =>
            defMap = defMap updated(symbolStr, a)
        }
        Result.Success(a)
      }
      case Result.Failure(location, message) => Result.Failure(location, message)
    }).filter(_.isInstanceOf[Result.Failure]).map {
      case Result.Failure(location, message) => (location, message)
    } match {
      case List() => Result.Success(this)
      case a => Result.Failure(a.head._1, a.mkString(","))
    }
  }

  def parse(symbol: String, input: String): Result[BnfParseResult] = defMap.get(symbol) match {
    case Some(f) => parse(f, input) match {
      case success: Result.Success[BnfParseResult] => success
      case Result.Failure(location, message) => Result.Failure(location, s"parse failed: ${message}")
    }
    case None => Result.Failure(Location(-1, -1), "parse failed: root parser not found")
  }

  def parse(input: String): Result[BnfParseResult] = parse("root", input)
}