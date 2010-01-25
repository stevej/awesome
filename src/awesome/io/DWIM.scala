package awesome
package io

import jvm._
import scala.tools.nsc.io._
import java.net.{ URL, MalformedURLException }

/** TODO - ant style regex, see FSRegex. */
trait DWIM[T] {
  private implicit def list2iterator[T](xs: List[T]): Iterator[T] = xs.iterator
  
  def fromBytes: Seq[Byte] => Iterator[T] = _ => Nil
  def fromString: String => Iterator[T] = _ => Nil
  def fromURL: URL => Iterator[T] = _ => Nil
  def fromFile: File => Iterator[T] = _ => Nil
  def fromDirectory: Directory => Iterator[T] = _ => Nil
  
  def toURL(s: String) = try Some(new URL(s)) catch { case _: MalformedURLException => None }
  def toFile(s: String) = if (Path(s).isFile) Some(File(s)) else None
  def toDirectory(s: String) = if (Path(s).isDirectory) Some(Directory(s)) else None
  
  def ops: List[String => Iterator[T]] = List(
    fromString,
    x => toURL(x).iterator flatMap fromURL,
    x => toFile(x).iterator flatMap fromFile,
    x => toDirectory(x).iterator flatMap fromDirectory
  )

  def transforms: List[String => Option[String]] = List(x => Some(x))
  
  def dwim(s: String): Iterator[T] = {
    for (op <- ops ; transform <- transforms ; arg <- transform(s)) {
      val it = op(arg)
      if (it.hasNext)
        return it
    }
    Iterator.empty
  }
  
  def apply(xs: String*): Iterator[T] = xs.iterator flatMap dwim    
}
