package awesome
package examples

import org.objectweb.asm._
import commons.{ GeneratorAdapter, Method }
import java.io.PrintStream
import Opcodes._
import asm._

// ------------------------------------------------------------------------
// Same example with a GeneratorAdapter (more convenient but slower)
// ------------------------------------------------------------------------
class Generator extends ClassLoader with ExampleCommon {
  def className = "Example2"
  val code = OpenClass(ClassWriter.COMPUTE_MAXS) { cw =>
    cw.initialize(className)

    // creates a GeneratorAdapter for the (implicit) constructor
    var m: Method = "void <init> ()"
    var mg = new GeneratorAdapter(ACC_PUBLIC, m, null, null, cw)
    mg.loadThis();
    mg.invokeConstructor(classOf[Object], m);
    mg.returnValue();
    mg.endMethod();

    // creates a GeneratorAdapter for the 'main' method
    // m = "void main (String[])"
    // mg = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, m, null, null, cw);
    mg = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC, "void main (String[])", null, null, cw);
    mg.getStatic(classOf[System], "out", classOf[PrintStream]);
    mg.push("Hello world!");
    mg.invokeVirtual(classOf[PrintStream],"void println (String)")
    mg.returnValue();
    mg.endMethod();
  }
}

object Generator {
  def main(args: Array[String]): Unit = {
    new Generator().execute
  }
}
