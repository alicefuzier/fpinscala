package fpinscala.parsing

import fpinscala.parsing.Representation.Parser
import fpinscala.testing.Prop._
import fpinscala.testing.{Gen, Prop}

import language.higherKinds
import scala.util.matching.Regex

object Representation {
  type Parser[+A] = Location => Result[A]
}

trait Result[+A]{
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e,isCommitted) => Failure(f(e),isCommitted)
    case _ => this
  }
  def uncommit: Result[A] = this match {
    case Failure(e,true) => Failure(e,false)
    case _ => this
  }
  def addCommit(isCommitted: Boolean): Result[A] = this match {
    case Failure(e,c) => Failure(e, c || isCommitted)
    case _=> this
  }

  def advanceSuccess(n: Int): Result[A] = this match {
    case Success(a,m) => Success(a,n+m)
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError,isCommitted: Boolean) extends Result[Nothing]

trait Parsers[+_] { self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Result[A] = p(Location(input,0))
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]= s => p(s) match {
    case Success(a,n) => f(a)(s.advanceBy(n))
      .addCommit(n != 0)
      .advanceSuccess(n)
    case f@Failure(_,_) => f
  }

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = l => p1(l) match {
    case Failure(e1,false) => p2(l)
    case s => s
  }
  def attempt[A](p: Parser[A]): Parser[A]=s => p(s).uncommit
  implicit def regex(r: Regex): Parser[String] = l => {
    r.findPrefixOf(l.input) match {
      case Some(matching) => Success (matching, matching.length)
      case None => Failure(l.toError(r + "does not match "+ l.input),false)
    }
  }
  implicit def string(s: String): Parser[String] = l => {
    if(l.input startsWith s) Success(l.input, s.length) else Failure(l.toError("Expected " + s),false)
  }
  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)
  def slice[A](p: Parser[A]): Parser[String] = l => {
    p(l) match{
      case Success(get,charsConsumed) => Success(l.input.substring(l.offset, l.offset + charsConsumed),charsConsumed)
      case f@Failure(_,_) => f
    }
  }
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.push(l, msg))
  def label[A](msg: String)(p: Parser[A]): Parser[A] = l => p(l).mapError(_.label(l, msg))

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))
  def map[A,B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(x=>succeed(f(x)))
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = flatMap(p)(a=> p2.map(b=>(a,b)))


  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = {
    (p ** p2) map{ case (a,b) => f(a,b)}
  }

  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C]= p.flatMap(a=>p2.map(b=>f(a,b)))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p,many(p))((a,b)=> a::b)

  val p1: Parser[Int] = "[0-9]".r.flatMap(digit=> listOfN(digit.toInt,char('a')).map(_=> digit.toInt))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2) // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def product[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""

  //    def errorLocation(e: ParseError): Location
  //    def errorMessage(e: ParseError): String
}

case class ParseError(stack: List[(Location,String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)
  def label(loc: Location, msg: String): ParseError =
    copy(stack = List((loc,msg)))
}