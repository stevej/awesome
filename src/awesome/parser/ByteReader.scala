package awesome
package parser

import scala.util.parsing.input.Reader
import scala.util.control.ControlException

object ParserEOFException extends ControlException

class ByteReader(val bytes: Array[Byte], override val offset: Int) extends Reader[Byte] {
  def this(reader: Reader[_]) = this(reader.source.toString.getBytes, 0)
  def this(bytes: Seq[Byte]) = this(bytes.toArray, 0)
  def this(str: String) = this(str.getBytes, 0)

  override def source = bytes map (_.toChar)
  
  def first: Byte =
    if (offset < bytes.length) bytes(offset)
    else throw ParserEOFException
    
  def rest: ByteReader = if (offset < bytes.length) new ByteReader(bytes, offset + 1) else this
  def pos = ByteOffsetPosition(offset)
  def atEnd = offset >= bytes.length
  
  def byteAt(n: Int) = bytes(n)
  def length = bytes.length - offset
  
  override def drop(n: Int): ByteReader = new ByteReader(bytes, offset + n)
  def take(n: Int): Seq[Byte] = bytes drop offset take n
  
  override def toString = {
    def isPrintable(c: Byte) = c >= 33 && c <= 126
    val text =
      if (bytes forall isPrintable) new String(bytes.toArray, "ISO-8859-1")
      else if (bytes.length < 20) bytes.mkString("(", ", ", ")")
      else "<unprintable>"
    "ByteReader(%d / %d) %s".format(offset, bytes.length, text)
  }
}
