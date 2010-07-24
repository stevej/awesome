package awesome
package parser

import scala.util.parsing.input.Position

case class ByteOffsetPosition(offset: Int) extends Position {
  final val line = 1
  def column = offset + 1
  def lineContents: String = ""
}
