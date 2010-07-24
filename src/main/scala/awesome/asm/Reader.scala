package awesome
package asm

import org.objectweb.asm._
import org.objectweb.asm.util.{ ASMifierClassVisitor, TraceMethodVisitor, CheckSignatureAdapter, CheckClassAdapter }
import org.objectweb.asm.signature.{ SignatureVisitor, SignatureReader }
import commons.EmptyVisitor
import visitor._
import Opcodes._
import awesome.jvm.{ Descriptor, Signature, FieldSignature, MethodSignature, ClassSignature }
import awesome.io.Jars

object Model {
  case class MethodInsn(opcode: Int, owner: String, name: String, desc: String) { }
  case class EnclosingClass(owner: String, name: String, desc: Descriptor) { }

  case class Class(
    access: Int, 
    name: String,
    signatureText: String = "",
    superName: String = "??",
    interfaces: List[String] = Nil,
    methods: List[Method] = Nil,
    fields: List[Field] = Nil,
    classes: List[Class] = Nil
  )

  case class Field(
    access: Int,
    name: String,
    desc: Descriptor,
    signatureText: String = "",
    value: Object = null
  )

  case class Flags(flags: Int) { }

  case class InnerClass(
    name: String, 
    outerName: String, 
    innerName: String,
    access: Int
  )

  case class Method(
    owner: Option[Class],
    access: Int, 
    name: String, 
    desc: Descriptor, 
    signatureText: String = "",
    exceptions: List[String] = Nil
  )
}
import Model._

class Reader(val name: String, val clazz: JClass[_]) {
  val cr = new ClassReader(name)

  def runVisitor[T](f: ListBuffer[T] => ClassVisitor): List[T] = {
    val buf = new ListBuffer[T]
    cr.accept(f(buf), 0)
    buf.toList
  }
  
  def jclass: Class = runVisitor[Class](
    buf => new EmptyClass {
      private var cs = new ListBuffer[Class]
      private var ms = new ListBuffer[Method]
      private var fs = new ListBuffer[Field]
      private var jclass: Class = null
      
      override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) { 
        jclass = Class(access, name, signature, superName, arrayToList(interfaces))
      }
      override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) {
        cs += Class(access, name)
      }
      override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        val method = Method(Some(jclass), access, name, desc, signature, arrayToList(exceptions))
        val mv = new EmptyMethod {
          private var insns = new ListBuffer[MethodInsn]
          override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) {
            insns += MethodInsn(opcode, owner, name, desc)
          }
          override def visitEnd() {
            // method.methodInsns = insns.toList
          }
        }
        ms += method
        mv
      }
      override def visitField(access: Int, name: String, desc: String, signature: String, value: Object): FieldVisitor = {
        fs += Field(access, name, desc, signature, value)
        null
      }
      override def visitEnd() {
        buf += jclass.copy(methods = ms.toList, fields = fs.toList, classes = cs.toList)
      }
    }
  ).head
  
  def attributes() = runVisitor[Attribute](
    buf => new EmptyVisitor { override def visitAttribute(attr: Attribute) = buf += attr }
  )
  
  def innerClasses() = runVisitor[InnerClass](
    buf => new EmptyVisitor { 
      override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) =
        buf += InnerClass(name, outerName, innerName, access)
    }
  )
  
  def methodCalls() = runVisitor[MethodCall](
    buf => new EmptyVisitor {
      override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) {
        super.visitMethodInsn(opcode, owner, name, desc)
        buf += MethodCall(opcode, owner, name, desc)
      }
    }
  )
  
  def methods() = runVisitor[Method](
    buf => new EmptyVisitor {
      override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        val ret = super.visitMethod(access, name, desc, signature, exceptions)
        buf += Method(None, access, name, desc, signature, arrayToList(exceptions))
        ret
      }
    }
  )
  
  def fields() = runVisitor[Field](
    buf => new EmptyVisitor {
      override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
        val ret = super.visitField(access, name, desc, signature, value)
        buf += Field(access, name, desc, signature, value)
        ret
      }
    }
  )
  
  def signatures() = runVisitor[Signature](
    buf => new EmptyVisitor {
      override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) = {
        val ret = super.visit(version, access, name, signature, superName, interfaces)
        if (signature != null)
          buf += ClassSignature(signature)
        
        ret
      }
      
      override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        val ret = super.visitMethod(access, name, desc, signature, exceptions)
        if (signature != null)
          buf += MethodSignature(signature)
        ret
      }
      override def visitField(access: Int, name: String, desc: String, signature: String, value: Object) = {
        val ret = super.visitField(access, name, desc, signature, value)
        if (signature != null)
          buf += FieldSignature(signature)
        ret
      }
    }
  )

  def enclosing() = runVisitor[EnclosingClass](
    buf => new EmptyVisitor {
      override def visitOuterClass(owner: String, name: String, desc: String) = {
        if (owner != null)
          buf += EnclosingClass(owner, name, desc)
      }
    }
  )
  
  def sourcefile() = runVisitor[String](
    buf => new EmptyVisitor {
      override def visitSource(source: String, debug: String) =
        if (source != null)
          buf += source
    }
  )
}

object Reader {
  private def outWriter = new java.io.PrintWriter(System.out)
  
  def apply(name: String) = new Reader(name, name.clazz)
  def apply(jclass: JClass[_]) = new Reader(jclass.getName, jclass)
  
  def allnames = Jars.default.classNames
  def all = allnames map Reader.apply
  def allsigs = all.toList flatMap (_.signatures)
  def allsigreaders = allsigs map (x => new SignatureReader(x.text))
  
  def dump(name: String) = CheckClassAdapter.verify(new ClassReader(name), true, outWriter)
  def parse(name: String) = println(new ClassReader(name) getClassName)

  def main(args: Array[String]): Unit = {
    args foreach parse
  }
}



