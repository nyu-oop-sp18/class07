package parsing

import scala.language.implicitConversions
import scala.util.Try
import scala.util.matching.Regex

trait Location

object Location {
  implicit def fromString(s: String): Location = ???
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

private[parsing] sealed trait ParseState[+A]

trait Parser[A] {
  protected def apply(loc: Location): ParseState[A]

  def parse(loc: Location): Try[A] = ???
  
  def orElse[A](p: Parser[A]): Parser[A] = ???

  def andThen[B](p: Parser[B]): Parser[(A, B)] = 
    for {
      a <- this
      b <- p
    } yield (a, b)
  
  def map[B](f: A => B): Parser[B] = ???
  
  def flatMap[B](f: A => Parser[B]): Parser[B] = ???
}

object Parser {
  def repeat[A](p: Parser[A]): Parser[List[A]] = ???

  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] = ???

  def attempt[A](p: Parser[A]): Parser[A] = ???

  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  def tag[A](msg: String)(p: Parser[A]): Parser[A] = ???

  implicit def char(c: Char): Parser[Char] = ???
  
  implicit def string(s: String): Parser[String] = ???
  
  implicit def regex(r: Regex): Parser[String] = ???

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toInt)
  
  def digits: Parser[Int] = repeat(digit) map (_ reduce (_ * 10 + _))
}


