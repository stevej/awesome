package awesome
package io

import java.util.jar._
import jvm.{ ClassFile, ParsedClassFile, ClassFileParser, ParsedCache, ProcessedCache }
import scala.tools.nsc.io.{ Path, File, Directory }
import scala.util.matching.Regex
import scala.util.Properties.javaClassPath
import java.io.File.pathSeparatorChar

case class JarInfo(name: String, size: Long) { }

object Jars {
  def default: Jars = classpath ++ bootClasspath
  def empty: Jars = new Jars(Nil)
  
  lazy val classpath: Jars = fromClasspathString(javaClassPath)
  lazy val bootClasspath: Jars = Option(System.getProperty("sun.boot.class.path")) map fromClasspathString getOrElse empty
  lazy val scala: Jars = fromClasspathString(javaClassPath) grep { x =>
    Set("scala-library.jar", "scala-library.jar") exists (x.name contains _)
  }
  
  implicit def fromDirectory(x: Directory): Jars =
    fromDirectory(x, true)
    
  def fromDirectory(x: Directory, deep: Boolean): Jars = {
    val jarnames = (x.deepList() filter (_.extension == "jar")).toList
    fromPaths(jarnames: _*)
  }
    
  def fromClasspathString(s: String): Jars =
    fromStrings(s split pathSeparatorChar filter (_ endsWith ".jar"): _*)

  def fromPaths(jars: Path*): Jars = fromStrings(jars map (_.path) : _*)
  def fromStrings(jars: String*): Jars = fromJars(jars.toList flatMap (x => Jar(x)) : _*)
  def fromJars(jars: Jar*): Jars = new Jars(jars.toList)

  def apply(x: Directory): Jars = fromDirectory(x)

  implicit def jarEntry2String(x: JarEntry): String = x.getName.stripSuffix(".class")
}
import Jars._

class Jars(
  val allJars: List[Jar],
  val jarFilter: Jar => Boolean = _ => true
) {
  
  def copy(jars: List[Jar] = allJars, filter: Jar => Boolean = jarFilter): Jars =
    new Jars(jars, filter)

  def entries: Iterator[JarEntry] = jars flatMap (_.entries)
  def foreach[T](f: JarEntry => T): Unit = entries foreach f
  
  def clear: Jars = new Jars(allJars map (_.clear))
  def jars: Iterator[Jar] = (allJars filter jarFilter).iterator
  def classNames = jars flatMap (_.classNames)
  def infos: Iterator[JarInfo] = entries map (x => JarInfo(x.getName, x.getSize))
  
  def find[T](p: T => Boolean)(implicit ev: Jar <%< T) = {
    val pp: Jar => Boolean = (x: Jar) => p(ev(x))
    jars find pp
  } 
  
  def grep(re: Regex): Jars         = grep(re findFirstMatchIn _.name isDefined)
  def grep(s: String): Jars         = grep(_.name contains s) 
  def grep(p: Jar => Boolean): Jars = copy(filter = ((x: Jar) => jarFilter(x) && p(x)))

  def ++(other: Jars): Jars =
    new Jars((allJars ++ other.allJars).removeDuplicates, (x: Jar) => jarFilter(x) && other.jarFilter(x))
  
  def >>[T](p: JarEntry => Iterator[T]): Iterator[T]  = entries flatMap p  
  def |[T](f: JarEntry => T): Iterator[T]             = entries map f
  
  def ??(re: Regex): Jars =
    ??[JarEntry](x => re findFirstMatchIn x.getName.stripSuffix(".class") isDefined)
  
  def ??[T](p: T => Boolean)(implicit ev: JarEntry <%< T) = {
    val pp: JarEntry => Boolean = (x: JarEntry) => p(ev(x))
    copy(jars = allJars map (_ addFilter pp))
  }
  
  def parsedClassFiles: Iterator[ParsedClassFile] = flattenIterator(classNames map ParsedCache.apply)
  def classFiles: Iterator[ClassFile] = flattenIterator(classNames map ProcessedCache.apply)
  def clazzes: Iterator[JClass[_]] = flattenIterator(classNames map (x => Ident(x).clazzOpt))
  
  override def toString = "Jars(%d/%d jars, %d entries before filtering)".format(
    jars.length, allJars.length, jars.toList map (_.file.size) sum
  )
}

