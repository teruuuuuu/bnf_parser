package jp.co.teruuu.converter.logging

import java.time.LocalDateTime

import com.github.kmizu.scomb.{Result, SCombinator}

sealed abstract class LogLine

case class LogLevel(val name: String)

case class Log4jLine(date: LocalDateTime, logLevel: LogLevel, name: String, thread: String, content: String) extends LogLine

sealed abstract class JValue
case class JObject(properties: (String, JValue)*) extends JValue
case class JArray(elements: JValue*) extends JValue
case class JString(value: String) extends JValue
case class JNumber(value: Double) extends JValue
case class JBoolean(value: Boolean) extends JValue
case object JNull extends JValue

object Log4jParser extends SCombinator {
  def root: Parser[Log4jLine] = for{
    _ <- DefaultSpace.*
    timestamp <- timestamp
    _ <- DefaultSpace.*
    logLevel <- logLevel
    _ <- DefaultSpace.*
    name <- name
    _ <- DefaultSpace.*
    thread <- thread
    _ <- DefaultSpace.*
    content <- any.* ^^ { case content => content.mkString}
  } yield Log4jLine(timestamp, logLevel, name, thread, content)

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

  lazy val LBRACKET = defaultToken("[")
  lazy val RBRACKET = defaultToken("]")
  lazy val LBRACE = defaultToken("{")
  lazy val RBRACE = defaultToken("}")
  lazy val COLON = defaultToken(":")
  lazy val COMMA = defaultToken(",")
  lazy val TRUE = defaultToken("true")
  lazy val FALSE = defaultToken("false")
  lazy val NULL = defaultToken("null")

  lazy val jvalue: P[JValue] = rule(jobject | jarray | jstring | jnumber | jboolean | jnull)

  lazy val jobject: P[JValue] = rule{for {
    _ <- LBRACE
    properties <- pair.repeat0By(COMMA)
    _ <- RBRACE.l("RBRACE")
  } yield JObject(properties:_*)}

  lazy val pair: P[(String, JValue)] = rule{for {
    key <- string
    _ <- COLON.l("COLON")
    value <- jvalue
  } yield (key, value)}

  lazy val jarray: P[JValue] = rule{for {
    _ <- LBRACKET
    elements <- jvalue.repeat0By(COMMA)
    _ <- RBRACKET.l("rbracket")
  } yield JArray(elements:_*)}

  lazy val string: Parser[String] = rule{for {
    _ <- $("\"")
    contents <- ($("\\") ~ any ^^ { case _ ~ ch => escape(ch).toString} | except('"')).*
    _ <- $("\"").l("double quote")
    _ <- DefaultSpace.*
  } yield contents.mkString}

  lazy val jstring: Parser[JValue] = rule(string ^^ {v => JString(v)})

  lazy val jnumber: Parser[JValue] = rule{for {
    value <- (set('0'to'9').+) ^^ { case digits => JNumber(digits.mkString.toInt) }
    _ <- DefaultSpace.*
  } yield value}

  lazy val jboolean: Parser[JValue] = rule(
    TRUE ^^ {_ => JBoolean(true)}
      | FALSE ^^ {_ => JBoolean(false)}
  )



  lazy val jnull: Parser[JValue] = rule(NULL ^^ {_ => JNull})

  lazy val timestamp: Parser[LocalDateTime] = rule{for {
    year <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string("-")
    month <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string("-")
    day <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string(" ")
    hour <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string(":")
    minute <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string(":")
    second <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
    _ <- string(",")
    mili <- (set('0'to'9').+) ^^ { case digits => digits.mkString.toInt }
  } yield LocalDateTime.of(year, month, day, hour, minute, second, mili * 1000)}

  lazy val logLevel: Parser[LogLevel] = rule{for {
    level <- rule(string("DEBUG") | string("INFO") | string("WARN")
      | string("ERROR") | string("FATAL")) ^^ {case level => LogLevel(level)}
  } yield level }

  lazy val name: Parser[String] = rule{for {
    name <- notSpace.+ ^^ {case name => name.mkString}
  } yield name }

  def notSpace: Parser[Char] = rule(for {
    _ <- not(string(" "))
    ch <- any
  } yield ch)

  lazy val thread: Parser[String] = rule{for {
    _ <- string("[")
    thread <- notBracket.+ ^^ {case thread => thread.mkString}
    _ <- string("]")
  } yield thread }

  def notBracket: Parser[Char] = rule(for {
    _ <- not(set(Seq('[', ']')))
    ch <- any
  } yield ch)

  def parse(input: String): Result[Log4jLine] = parse(root, input)
}
