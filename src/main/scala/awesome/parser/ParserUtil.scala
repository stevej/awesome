package awesome
package parser

import scala.util.parsing.combinator._

trait ParserUtil extends Parsers {
  lazy val anyElem: Parser[Elem]          = elem("anyElem", _ => true)
  def elemExcept(xs: Elem*): Parser[Elem] = elem("elemExcept", x => !(xs contains x))
  def elemOf(xs: Elem*): Parser[Elem]     = elem("elemOf", xs contains _)
  
  def take(n: Int): Parser[Seq[Elem]] = repN(n, anyElem)
  
  def takeUntil(cond: Parser[Elem]): Parser[Seq[Elem]] = takeUntil(cond, anyElem)
  def takeUntil(cond: Parser[Elem], p: Parser[Elem]): Parser[Seq[Elem]] = rep(not(cond) ~> p)
  
  def takeTo(cond: Parser[Elem]): Parser[Seq[Elem]] = takeTo(cond, anyElem)
  def takeTo(cond: Parser[Elem], p: Parser[Elem]): Parser[Seq[Elem]] =
    rep(not(cond) ~> p) ~ anyElem ^^ { case xs~x => xs :+ x }
  
  //
  def tuplify[A, B](x: ~[A, B]): (A, B) = x match { case a~b => ((a, b)) }
  def swap[A, B](x: ~[A, B]): ~[B, A]   = x match { case a~b => new ~(b, a) }
}
