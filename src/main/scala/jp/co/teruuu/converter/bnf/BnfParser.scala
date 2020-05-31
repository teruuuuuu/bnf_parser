package jp.co.teruuu.converter.bnf

import com.github.kmizu.scomb.{Location, Result, SCombinator}


object BnfParser extends SCombinator {

  case class SymbolDef(symbol: String, parser: Parser[String])

  var defMap = Map.empty[String, Parser[String]]

  def bnf_parser: Parser[SymbolDef] = for {
    _ <- DefaultSpace.*
    symbol <- symbol
    _ <- DefaultSpace.*
    _ <- $("::=")
    _ <- DefaultSpace.*
    definition <- definition
    _ <- DefaultSpace.*
  } yield SymbolDef(symbol, definition)

  lazy val symbol: Parser[String] = rule {
    for {
      a <- char
      b <- str
    } yield a + b
  }

  lazy val definition: Parser[Parser[String]] = rule {
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
        case None => c.foldLeft(a)((acc, cur) => (acc|cur))
        case Some("+") => c.foldLeft(a.+ ^^ (_.mkString))((acc, cur) => (acc|cur))
        case Some("*") => c.foldLeft(a.* ^^ (_.mkString))((acc, cur) => (acc|cur))
        case _ => sys.error("")
      })
  }.+ ^^ {case seq: List[Parser[String]] => seq.tail.foldLeft(seq.head)((acc,cur) => acc ~ cur ^^ {case a ~ b => a + b})}

  lazy val char: Parser[String] = rule {
    for {
      a <- (set('a' to 'z') | set('A' to 'Z'))
    } yield a
  }

  lazy val str: Parser[String] = rule {
    for {
      a <- (char | set('0' to '9')).* ^^ (_.mkString)
    } yield a
  }

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

  lazy val string: Parser[Parser[String]] = rule {
    for {
      _ <- $("\"")
      contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString } | except('"')).*
      _ <- $("\"").l("double quote")
    } yield $(contents.mkString)
  }

  lazy val optional: Parser[Parser[String]] = rule {
    for {
      _ <- $("[")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $("]")
    } yield a.? ^^ (_.mkString)
  }

  lazy val group: Parser[Parser[String]] = rule {
    for {
      _ <- $("(")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $(")")
    } yield a
  }

  lazy val repeat: Parser[Parser[String]] = rule {
    for {
      _ <- $("{")
      _ <- DefaultSpace.*
      a <- definition
      _ <- DefaultSpace.*
      _ <- $("}")
    } yield a.* ^^ (_.mkString)
  }

  lazy val defined_symbol: Parser[Parser[String]] = rule {
    for {
      a <- symbol ^^ (symbol =>
        defMap.getOrElse(symbol, sys.error("symbol not defined")))
    } yield a
  }

  def load_bnf(definitions: List[String]): List[Result[SymbolDef]] = {
    defMap = initial_symbol
    definitions.foldLeft(List.empty[Result[SymbolDef]])((acc, cur) =>
      parse(bnf_parser, cur) match {
        case Result.Success(SymbolDef(symbol, definitions)) => if (defMap.contains(symbol)) {
          Result.Failure(Location(0, 0), "gen_parser failed: symbol already defined") :: acc
        } else {
          defMap = defMap updated(symbol, definitions)
          Result.Success(SymbolDef(symbol, definitions)) :: acc
        }
        case Result.Failure(location, message) =>
          Result.Failure(location, "gen_parser failed: " + message) :: acc
      }
    )
  }

  def parse(input: String): Result[String] = defMap.get("root") match {
    case Some(f) => parse(f, input) match {
      case success: Result.Success[String] => success
      case Result.Failure(location, message) => Result.Failure(location, s"parse failed: ${message}")
    }
    case None => Result.Failure(Location(-1, -1), "parse failed: root parser not found")
  }

  def initial_symbol: Map[String, Parser[String]] = Map(
    "sp" -> rule(DefaultSpace.+ ^^ (_ => " ")),
    "notsp" -> rule(except(' ').+ ^^ (a => a.mkString)),
  )
}
