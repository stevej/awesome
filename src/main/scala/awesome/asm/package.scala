package awesome

import org.objectweb.asm._
import Opcodes._
import java.lang.reflect
import scala.collection.JavaConversions

package object asm {
  // ASM type creation
  implicit def string2asmMethod(s: String): commons.Method = commons.Method.getMethod(s)
  implicit def javaMethod2asmMethod(m: reflect.Method): commons.Method = commons.Method.getMethod(m)
  implicit def class2asmType(clazz: JClass[_]): Type = Type.getType(clazz)
  
  // Awesome type creation
  implicit def classWriter2OpenClass(cw: ClassWriter): OpenClass = new OpenClass(cw)
  implicit def string2openMethod(owner: String)(implicit mw: MethodVisitor): OpenMethod = new OpenMethod(owner)
  implicit def class2openMethod(owner: JClass[_])(implicit mw: MethodVisitor): OpenMethod = new OpenMethod(owner.getName.toInternal)
  
  //
  private[asm] implicit def typedList[T](x: java.util.List[_]): List[T] =
    JavaConversions.asBuffer(x).toList.asInstanceOf[List[T]]
}
