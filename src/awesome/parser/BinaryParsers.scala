package awesome
package parser

import scala.util.parsing.combinator._
import scala.io.Codec
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble
import scala.annotation.tailrec

/** This trait has only minimal changes to the stdlib to make binary
 *  parsers workable.
 */
trait BinaryParsers extends Parsers {
  type Elem = Byte
  
  /** The standard library parsers are written with the assumption that end of stream
   *  can be recognized with inband data (i.e. EofCh) which is not true for binary data.
   *  So we are forced to override the methods which may encounter end of stream and
   *  throw a control exception, then catch that and translate it to parse Failure.
   */
  private def eofWrapper[T](p: Parser[T], alt: Input => ParseResult[T]): Parser[T] = Parser { in =>
    try p(in)
    catch { case ParserEOFException => alt(in) }      
  }

  override def acceptIf(p: Elem => Boolean)(err: Elem => String): Parser[Elem] =
    eofWrapper[Elem](super.acceptIf(p)(err), Failure("EOF", _))

  override def acceptMatch[U](expected: String, pf: Elem =>? U): Parser[U] =
    eofWrapper[U](super.acceptMatch(expected, pf), Failure("EOF", _))
}

/** As there are at least a few ways to interpret byte patterns
 *  as Ints and other primitives, we abstract this out.
 */
trait ByteInterpretation extends BinaryParsers {
  def toInt(bytes: Seq[Byte]): Int
  def toLong(bytes: Seq[Byte]): Long
  
  /** Even this isn't a sure thing thanks to "Modified UTF-8" */
  def toUTF8String(bytes: Seq[Byte]) = Codec toUTF8 bytes.toArray mkString
  
  /** A reasonable supposition */
  def toFloat(xs: Seq[Byte]) = intBitsToFloat(toInt(xs))
  def toDouble(xs: Seq[Byte]) = longBitsToDouble(toLong(xs))
}

/** Eight bits to a byte, four bytes to an Int, etc.
 */
trait StandardByteInterpretation extends ByteInterpretation {
  def toInt(xs: Seq[Byte]): Int = xs.foldLeft(0)((x, b) => (x << 8) + (b & 0xFF))
  def toLong(xs: Seq[Byte]): Long = xs.foldLeft(0L)((x, b) => (x << 8) + (b & 0xFF))
}

/** Variable number of bytes, high bit used to signal end of number.
 */
trait PicklerByteInterpretation extends ByteInterpretation {
  def toInt(xs: Seq[Byte]): Int = xs.foldLeft(0)((x, b) => (x << 7) + (b & 0x7F))
  def toLong(xs: Seq[Byte]): Long = xs.foldLeft(0L)((x, b) => (x << 7) + (b & 0x7F))  
}

trait StandardBinaryParsers extends BinaryParsers with ByteInterpretation with ParserUtil {
  lazy val u1: Parser[Int] = byte ^^ (_ & 0xFF)
  lazy val u2: Parser[Int] = bytes(2) ^^ toInt
  lazy val u4: Parser[Int] = bytes(4) ^^ toInt
  lazy val u2c: Parser[Char] = bytes(2) ^^ toInt ^^ (_ & 0xFFFF) ^^ (_.toChar)
  lazy val u4f: Parser[Float] = u4 ^^ intBitsToFloat
  lazy val u8: Parser[Long] = bytes(8) ^^ toLong
  lazy val u8d: Parser[Double] = u8 ^^ longBitsToDouble
  
  /** Parse a single byte. */
  lazy val byte: Parser[Byte] = anyElem

  /** Parse a fixed number of bytes. */
  def bytes(n: Int): Parser[Seq[Byte]] = Parser { in =>
    if (n <= in.length) Success(in take n, in drop n)
    else Failure("Requested %d bytes but only %d remain".format(n, in.length), in)
  }
  
  protected implicit def readerToByteReader(x: Input): ByteReader = x match {
    case br: ByteReader => br
    case _              => new ByteReader(x)
  }
  
  /** Parse all bytes up to end of stream. */
  lazy val drain: Parser[Seq[Byte]] = Parser { in =>
    Success(in take in.length, in drop in.length)
  }

  /** Convenience parsing functions. */
  def parse[T](p: Parser[T], in: Seq[Byte]): ParseResult[T] = parse(p, new ByteReader(in))
  def parse[T](p: Parser[T], in: Input): ParseResult[T] = p(in)
  def parse[T](p: Parser[T], in: String): ParseResult[T] = parse(p, new ByteReader(in))
    
  def parseAll[T](p: Parser[T], in: Seq[Byte]) = parse(phrase(p), in)
}