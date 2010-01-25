package awesome
package io

import java.util.jar._
import scala.tools.nsc.io.{ Path, File, Directory }
import jvm.{ ParsedCache, ProcessedCache }

  
case class ReferencedClassGroup(
  file: JarFile,
  internal: List[String],
  java: List[String],
  external: List[String]
) {
  private def total = internal.size + java.size + external.size
  lazy val (scala, nonScala) = external partition (_ startsWith "scala/")
  def dependencies = nonScala uniqmap (_.toPackage)
  
  override lazy val toString = "%s(%d referenced classes: %d in jar, %d java, %d external)".format(
    file.size, total, internal.size, java.size, external.size
  )
}

class Jar(
  val file: JarFile,
  val entryFilter: JarEntry => Boolean = _ => true
) extends Iterable[JarEntry] {
  import scala.collection.JavaConversions._
  
  def name = file.getName
  def clear: Jar = new Jar(file)
  def iterator = (file.entries(): Iterator[JarEntry]) filter entryFilter
  def entries = iterator
  def entry(name: String): Option[JarEntry] = Option(file getJarEntry name)

  private def classNameFilter: (String =>? String) = {
    case x if x endsWith ".class" => x dropRight 6
  }  
  def classNames: Iterator[String] = iterator map (_.getName) partialMap classNameFilter
  
  def parsedClassFiles = classNames flatMap ParsedCache.apply
  def classFiles = classNames flatMap ProcessedCache.apply
  
  def referencedClasses = classFiles uniqfmap (_.referencedClasses)
  
  def referenced = {
    val names = Set(classNames.toList: _*)
    val lb1, lb2, lb3, lb4 = new ListBuffer[String]
    
    classFiles uniqfmap (_.referencedClasses) filterNot (_ startsWith "[") foreach { x =>
      if (names(x)) lb1 += x
      else if (x startsWith "java/") lb2 += x
      else lb3 += x
    }
    
    ReferencedClassGroup(file, lb1.toList, lb2.toList, lb3.toList)
  }
  
  def addFilter(p: JarEntry => Boolean): Jar =
    new Jar(file, (x: JarEntry) => entryFilter(x) && p(x))
}

object Jar {
  import java.util.zip.ZipException
    
  def apply(path: String): Option[Jar] = apply(Path(path))
  def apply(path: Path): Option[Jar] =
    try Some(new Jar(new JarFile(path.path)))
    catch { case _: ZipException => None }
}
