package awesome

import io.Jars
import pickler.{ Show, ScalaSig, ScalaSigParser }
import java.util.jar.JarEntry

object Main
{
  /** ClassFile */
  def cf(name: String) = pcf(name).process
  def cfs(name: String) = CF(name).toList
  
  /** ParsedClassFile */
  def pcf(name: String) = PCF.one(name).get
  def pcfs(name: String) = PCF(name).toList
  
  /** ScalaSig */
  def sig(name: String) = cf(name).scalaSig.get
  def sigs(name: String) = cfs(name) flatMap (_.scalaSig)
  def allSigs(name: String) = scala2 flatMap (_.scalaSig)
  
  /** List[ClassFile] grepping scala jars */
  def sgrep(s: String) = Jars.scala ?? ((x: JarEntry) => x.getName contains s) classFiles
  
  /** ... grepping all jars. */
  def cgrep(s: String) = Jars.default ?? ((x: JarEntry) => x.getName contains s) classFiles

  /** Show scala sig using scala's ShowPickled */
  def show1(name: String) = Show(name)
  
  /** ... using ours. */
  def show2(name: String) = sig(name).show
  
  /** All scala. */
  def scala1 = Jars.scala.parsedClassFiles
  def scala2 = Jars.scala.classFiles
  // { x: JarEntry => !(x.getName contains "RedBlack") }
  
  /** All method descriptors from classes matching string */
  def descriptors(s: String) = cgrep(s).toList flatMap (_.allMethods) map (_.descriptor.text) uniqsort

  def main(args: Array[String]): Unit = {
    
  }
}
