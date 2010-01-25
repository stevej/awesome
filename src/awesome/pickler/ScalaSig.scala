package awesome
package pickler

import Constants._
import parser.ByteReader
import jvm.ClassFileParser

case class Version(major: Int, minor: Int) { }

class ScalaSig(
  val context: Pool#PoolWithContext,
  val version: Version
) {
  import context.{ pool, entries }
  type Entry = Pool#Entry

  def iterator = entries.zipWithIndex map (_.swap) iterator
  def toList = iterator.toList
  
  def showEntry(pair: (Int, Entry)) = pair match { case (k, v) => "%d: %s (%s)".format(k, v, Constants(v.tag)) }
  def show = iterator map showEntry foreach println

  def wrapAt(width: Int)(xss: Seq[Any]*): String = {
    val sb = new StringBuilder
    var cur = 0
    for (xs <- xss ; x <- xs) {
      val str = x.toString
      if ((cur + str.length) >= width) {
        sb append "\n"
        cur = 0
      }
      cur += (str.length + 1)
      sb append str
      sb append " "
    }
    sb.toString.trim
  }
  
  def toLongString = (iterator map showEntry).mkString("", "\n", "\n")
  
  def toSummaryString = List(
    this.toString,
    wrapAt(70)(Seq("Term names: "), context.termNames),
    "",
    wrapAt(70)(Seq("Type names: "), context.typeNames)
  ).mkString("\n")

  override def toString = "ScalaSig(%d entries)".format(entries.size)
}

object ScalaSig {
  def getBytes(name: String): Array[Byte] = (
    for {
      cp <- ClassFileParser(name).toOption
      attr <- cp.process.scalaSigAttr
    } yield attr.bytes.toArray
  ) getOrElse Array()      
}