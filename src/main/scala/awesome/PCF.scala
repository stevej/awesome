package awesome

import jvm._
import scala.tools.nsc.io._
import awesome.io.{ Jar, DWIM }
import java.net.{ URL, MalformedURLException }

/* XXX remove duplicated CF/PCF */
object PCF extends DWIM[ParsedClassFile] {  
  override def fromBytes = (xs: Seq[Byte]) => ClassFileParser opt xs
  
  override def fromFile = (x: File) => x.extension match {
    case "jar"    => Jar(x).iterator flatMap (_.parsedClassFiles)
    case "class"  => fromBytes(x.bytes().toSeq)
    case _        => Iterator.empty
  }
    
  override def fromDirectory = (x: Directory) => x.files flatMap fromFile
  override def fromString = (x: String) => {
    val res = ClassFileParser opt x
    if (res.isDefined) res.iterator
    else {
      val path = Path(x)
      val parent = path.parent.toDirectory
      if (parent.isValid) {
        val regexp = path.name.replaceAll("""\*""", ".*").r
        val jars = parent.files filter (x => regexp findFirstMatchIn x.name isDefined)
        jars flatMap fromFile
      }
      else Iterator.empty
    }
  }
  
  def classNameFilter(s: String) = if (s endsWith ".class") Some(s dropRight 6) else None
  def externalNameFilter(s: String) = if (s contains "/") Some(s.replace('/', '.')) else None

  override def transforms = super.transforms ::: List(
    classNameFilter _,
    externalNameFilter _
  )
  
  def one(name: String): Option[ParsedClassFile] = ClassFileParser opt name
}
