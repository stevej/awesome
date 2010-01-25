package awesome
package cmd

import scala.tools.nsc.io.File
import io.{ Jar, Jars }
import jvm.ClassFileParser
import jvm.attr.{ InnerClasses, InnerClass }
import scala.tools.nsc.io._
import awesome.io.ByteCode
import awesome.jvm.ClassFileParser

object javap {
  
  def usageMsg = """
    |Usage: javap <options> <classes>...
    |
    |where options include:
    |   -c                        Disassemble the code
    |   -classpath <pathlist>     Specify where to find user class files
    |   -extdirs <dirs>           Override location of installed extensions
    |   -help                     Print this usage message
    |   -J<flag>                  Pass <flag> directly to the runtime system
    |   -l                        Print line number and local variable tables
    |   -public                   Show only public classes and members
    |   -protected                Show protected/public classes and members
    |   -package                  Show package/protected/public classes
    |                             and members (default)
    |   -private                  Show all classes and members
    |   -s                        Print internal type signatures
    |   -bootclasspath <pathlist> Override location of class files loaded
    |                             by the bootstrap class loader
    |   -verbose                  Print stack size, number of locals and args for methods
    |                             If verifying, print reasons for failure
    |""".trim.stripMargin

  def usage = Console println usageMsg
  
  def main(args: Array[String]): Unit = {
    if (args.isEmpty || (args contains "-help"))
      return usage
      
    val (paths, others) = args.toList partition (x => Path(x).exists)
    val targets: List[(String, List[Byte])] =
      (paths map (x => (x, File(x).bytes().toList))) :::
      (others map (x => (x, ByteCode(x).toList)))

    val classFiles = targets map (x => ClassFileParser(x._2).toOption)
    classFiles.flatten map (_.process) foreach println
  }
}
