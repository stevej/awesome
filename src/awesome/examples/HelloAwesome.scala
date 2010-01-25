package awesome
package examples

import org.objectweb.asm.{ ClassWriter, MethodVisitor, Opcodes, Type }
import org.objectweb.asm.commons.{ GeneratorAdapter, Method }
import java.io.{ FileOutputStream, PrintStream }
import Opcodes._
import asm._

object HelloAwesome {
  def main(args: Array[String]): Unit = {
    new HelloAwesome().execute
  }
}

trait ExampleCommon extends ClassLoader {
  def className: String
  def code: Array[Byte]
  def execute = {
    writeClass
    val exampleClass = super.defineClass(className, code, 0, code.length)
    // uses the dynamically generated class to print 'Helloworld'
    for (mm <- exampleClass.getMethods find (_.getName == "main"))
      mm.invoke(null, Array[String](null))
  }
  
  def writeClass = {
    val fos = new FileOutputStream(className + ".class");
    fos.write(code);
    fos.close();
  }
}

// Generates the jvm corresponding to the following Java class:
// 
// public class Example {
//   public static void main (String[] args) {
//     System.out.println("Hello world!");
//   }
// }
class HelloAwesome extends ClassLoader with ExampleCommon {
  def className = "HelloAwesome"
  val code = OpenClass() { cw =>
    cw initialize className
    cw.defaultConstructor()
    
    cw mainMethod { implicit mw =>      
      // Or you could say "java/lang/System" getStatic [...]
      (classOf[System] getStatic "out" invokeVirtual "println")("Hello world!")
      (classOf[System] invokeStatic "setProperty")("propfoo", "valuebar")
    }
    
    cw.instanceMethod("foo", (x: String) => x.length) { implicit mw =>      
      mw.visitVarInsn(ALOAD, 1)
      ("java/lang/String" invokeVirtual "length")()
      mw visitInsn IRETURN
    }
  }
}
