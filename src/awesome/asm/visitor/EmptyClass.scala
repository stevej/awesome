package awesome
package asm
package visitor

import org.objectweb.asm._
import java.lang.reflect
import jvm.{ Attribute => _, _ }
import Model._

class PartialClass[T](pf: Any =>? T) extends ClassVisitor {
  private var buf = new ListBuffer[T]
  private var isComplete = false
  private lazy val bufResult = buf.toList
  
  def record[R >: Null](x: Any): R = {
    if (pf isDefinedAt x)
      buf += pf(x)
    
    null
  }
  
  def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit =
    record(Class(access, name, signature, superName, arrayToList(interfaces)))
    
  def visitSource(source: String, debug: String) { }
  def visitOuterClass(owner: String, name: String, desc: String) { }
  def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = null
  def visitAttribute(attr: Attribute) { }

  def visitInnerClass(name: String, outerName: String, innerName: String, access: Int): Unit =
    record(Class(access, name, null, null, null))

  def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor =
    record[FieldVisitor](Field(access, name, desc, signature, value))
    
  def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor =
    record[MethodVisitor](Method(None, access, name, desc, signature, arrayToList(exceptions)))
    
  def visitEnd() {
    synchronized {
      bufResult
      isComplete = true
    }
  }
  def apply(): List[T] = if (isComplete) bufResult else error("Not ready")
}

class EmptyClass extends ClassVisitor {    
  def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) { }
  def visitSource(source: String, debug: String) { }
  def visitOuterClass(owner: String, name: String, desc: String) { }
  def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = null
  def visitAttribute(attr: Attribute) { }
  def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) { }
  def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor = null
  def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = null
  def visitEnd() { }      
}
