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
