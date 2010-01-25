package awesome

import pickler.{ Show, ScalaSig, ScalaSigParser }
import jvm.ClassFileParser
import jvm.attr.{ InnerClasses, InnerClass }
import scala.tools.nsc.io.File
import java.lang.reflect
// import reflect.{ Proxy, InvocationHandler }
import io.Jars
// import io.Jar.{ scalaJars }

object Run {
  implicit def br(xs: Seq[Byte]) = new parser.ByteReader(xs)
  
  def parse(xs: Seq[Byte])(p: ScalaSigParser => ScalaSigParser#Parser[_]) = p(new ScalaSigParser())(br(xs))
  // 
  // def bcf2(path: String) = {
  //   val pcf = jvm.ClassFileParser(File(path).bytes().toSeq).get.process
  //   pcf.scalaSig.get
  // }
  // def bcf(path: String) = {
  //   val pcf = jvm.ClassFileParser(File(path).bytes().toSeq).get.process
  //   val xs = pcf.scalaSigAttr.get.bytes
  //   
  //   pickler.ScalaSigParser.justBytes(xs)
  // }
  //   
  // def rcf(name: String) = ClassFileParser(name).get
  // def pcf(name: String) = rcf(name).process
  // def sig(name: String) = pcf(name).scalaSig
  // def psig(name: String) = pcf(name).scalaSig.get
  // def jgrep(s: String) = Jars.default grep s
  // def cgrep(s: String) = {
  //   (Jars.scala grep s).classNames map { x =>
  //     try {
  //       println(x + " ...")
  //       pcf(x)
  //     }
  //     catch { 
  //       case e =>
  //         println("FAIL: '%s'".format(x))
  //         println(e)
  //         e.printStackTrace
  //         null
  //     }
  //   }
  // }
  // 
  // def show1(name: String) = Show(name)
  // def show2(name: String) = psig(name).show
  // 

  // def unpickleall = {
  //   import java.util.jar.JarEntry
  //    (Jars.scala ?? { x: JarEntry => !(x.getName contains "RedBlack") } ).classFiles
  // }
  // def allSigs = unpickleall.toList flatMap (_.scalaSig)
  
  // def allDescriptors = {
  //   def descGrouper(x: jvm.MethodInfo) = x.descriptor
  //   val xs = unpickleall.toList flatMap (_.allMethods) groupBy (_.descriptor.toString)
  //   
  //   val xs2 = xs.toList map { case (k, v) => (k, v map (_.name) removeDuplicates) }
  //   
  //   xs2 sortWith ((x, y) => x._2.size > y._2.size)
  // }
  
  // def allMembers = unpickleall flatMap (_.allMembers)
  // def allDescriptors = {
  //   def descGrouper(x: jvm.MethodInfo) = x.name.name + " " + x.descriptor
  //   val xs = unpickleall.toList flatMap (_.allMethods) groupBy descGrouper
  //   
  //   xs.toList sortWith ((x, y) => x._2.size > y._2.size)
  // }
  // def printAllDescriptors(xs: List[(String, List[_])]) = {
  //   xs foreach { case (k, v) => println(k + " " + v.size) }
  // }
}