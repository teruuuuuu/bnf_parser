package jp.co.teruuu.bnf

import com.github.kmizu.scomb.{Location, Result, SCombinator}

object BnfObject {
  type ParsedSymbol = Map[String, List[String]]

  object ParsedSymbol {
    def empty = Map.empty[String, List[String]]

    def append(a1: ParsedSymbol, a2: ParsedSymbol): ParsedSymbol =
      a2.foldLeft(a1)((acc, cur) =>
        acc updated(cur._1, acc.getOrElse(cur._1, List.empty[String]) ++ cur._2))

    def appendSeq(seq: List[ParsedSymbol]): ParsedSymbol = seq match {
      case head :: tail => tail.foldLeft(head)(append)
      case _ => empty
    }

    def string(str: String) = Map("_" -> List(str))

    def to_symbol(a: ParsedSymbol, symbol: String): ParsedSymbol =
      a updated(symbol, List(a.getOrElse("_", List()).mkString))

    def toJson(a: ParsedSymbol, m: ParsedSymbol) =
      s"${
        "{" + m.map(v =>
          "\"" + v._1.replace("\"", "\\\"")  + "\":" + (v._2.filter(k => a.contains(k)) match {
            case seq if seq.nonEmpty => "\"" + seq.map(a.getOrElse(_, List()).mkString(",")).mkString(",").replace("\"", "\\\"") + "\""
            case _ => "null"
          })
        ).mkString(",") + "}"
      }"
  }

  object BnfParser {
    def apply(definitions: List[String]):(List[Result[BnfParser#SymbolDef]], Option[BnfParser]) = BnfParser().load_bnf(definitions)
  }

  case class BnfParser() extends SCombinator {
    var defMap = Map.empty[String, Parser[ParsedSymbol]]

    def initial_symbol: Map[String, Parser[ParsedSymbol]] = Map(
      "sp" -> rule(DefaultSpace.+ ^^ (_ => Map("_" -> List(" ")))),
      "notsp" -> rule(except(' ').+ ^^ (a => Map("_" -> List(a.mkString)))),
    )

    case class SymbolDef(symbol: String, parser: Parser[ParsedSymbol])

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
        _ <- $("<")
        a <- char
        b <- str
        _ <- $(">")
      } yield a + b
    }

    lazy val definition: Parser[Parser[ParsedSymbol]] = rule {
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
        case Some("+") => c.foldLeft(a.+ ^^ {
          case l if l.isEmpty => ParsedSymbol.empty
          case x => ParsedSymbol.appendSeq(x)
        })((acc, cur) => acc | cur)
        case Some("*") => c.foldLeft(a.* ^^ {
          case l if l.isEmpty => ParsedSymbol.empty
          case x => ParsedSymbol.appendSeq(x)
        })((acc, cur) => acc | cur)
        case _ => sys.error("")
      })
    }.+ ^^ (a => a.tail.foldLeft(a.head)((acc, cur) =>
      acc ~ cur ^^ { case a ~ b => ParsedSymbol.append(a, b) }))

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

    lazy val string: Parser[Parser[ParsedSymbol]] = rule {
      for {
        _ <- $("\"")
        contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString } | except('"')).*
        _ <- $("\"").l("double quote")
      } yield $(contents.mkString) ^^ (a => ParsedSymbol.string(a))
    }

    lazy val optional: Parser[Parser[ParsedSymbol]] = rule {
      for {
        _ <- $("[")
        _ <- DefaultSpace.*
        a <- definition
        _ <- DefaultSpace.*
        _ <- $("]")
      } yield a.? ^^ {
        case Some(x) => x
        case _ => ParsedSymbol.empty
      }
    }

    lazy val group: Parser[Parser[ParsedSymbol]] = rule {
      for {
        _ <- $("(")
        _ <- DefaultSpace.*
        a <- definition
        _ <- DefaultSpace.*
        _ <- $(")")
      } yield a
    }

    lazy val repeat: Parser[Parser[ParsedSymbol]] = rule {
      for {
        _ <- $("{")
        _ <- DefaultSpace.*
        a <- definition
        _ <- DefaultSpace.*
        _ <- $("}")
      } yield a.* ^^ (ParsedSymbol.appendSeq)
    }

    lazy val defined_symbol: Parser[Parser[ParsedSymbol]] =
      symbol ^^ (symbol =>
        defMap.getOrElse(symbol, sys.error("symbol not defined")) ^^ (
          a => ParsedSymbol.to_symbol(a, symbol)))


    private def load_bnf(definitions: List[String]): (List[Result[SymbolDef]], Option[BnfParser]) = {
      defMap = initial_symbol
      val parseResult = definitions.foldLeft(List.empty[Result[SymbolDef]])((acc, cur) =>
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
      if (parseResult.exists(_.isInstanceOf[Result.Failure])) {
        (parseResult, None)
      } else {
        (parseResult, Some(this))
      }
    }

    def parse(symbol: String, input: String): Result[ParsedSymbol] = defMap.get(symbol) match {
      case Some(f) => parse(f, input) match {
        case success: Result.Success[ParsedSymbol] => success
        case Result.Failure(location, message) => Result.Failure(location, s"parse failed: ${message}")
      }
      case None => Result.Failure(Location(-1, -1), "parse failed: root parser not found")
    }

    def parse(input: String): Result[ParsedSymbol] = parse("root", input)
  }
}
