package awesome
package asm

import org.objectweb.asm._
import java.io.{ FileOutputStream, PrintStream }
import Opcodes._
import jvm.Descriptor

object OpenClass {
  def apply(flags: Int = COMPUTE_FRAMES)(f: ClassWriter => Unit): Array[Byte] = {
    val cw = new ClassWriter(flags)
    f(cw)
    cw.visitEnd()
    cw.toByteArray()
  }
}

class OpenClass(cw: ClassWriter) {
  def initialize(name: String,
    superName: String = JL_OBJECT,
    version: Int = DEFAULT_JVM_VERSION,
    access: Int = ACC_PUBLIC,
    signature: String = null,
    interfaces: Array[String] = null): Unit =
  {
    cw.visit(version, access, name, signature, superName, interfaces)
  }
  
  def constructor(access: Int, maxStack: Int, maxLocals: Int)(f: MethodVisitor => Unit) =
    method(access, "<init>", "()V") { mw => 
      // pushes the 'this' variable
      mw.visitVarInsn(ALOAD, 0)
      // invokes the super class constructor
      mw.visitMethodInsn(INVOKESPECIAL, JL_OBJECT, "<init>", "()V")
      // run supplied body
      f(mw)
      
      mw.visitInsn(RETURN)
      mw.visitMaxs(maxStack, maxLocals)
    }
  
  // this code uses a maximum of one stack element and one local variable
  def defaultConstructor() = constructor(ACC_PUBLIC, 1, 1)(_ => ())
  
  def mainMethod(f: MethodVisitor => Unit) = {
    def f2(mw: MethodVisitor) {
      f(mw)
      mw visitInsn RETURN
    }
    method(ACC_PUBLIC + ACC_STATIC, "main", "([Ljava/lang/String;)V")(f2)
  }
  
  def staticMethod(name: String, desc: String) =
    method(ACC_PUBLIC + ACC_STATIC, name, desc) _
  
  def instanceMethod(name: String, desc: Descriptor) =
    method(ACC_PUBLIC, name, desc.text) _
  
  def method(
        access: Int,
        name: String, 
        desc: String,
        signature: String = null,
        exceptions: Array[String] = null)(f: MethodVisitor => Unit) =
  {
    val mw = cw.visitMethod(access, name, desc, signature, exceptions)
    f(mw)
    mw.visitMaxs(0, 0)
    mw.visitEnd()
  }
}
