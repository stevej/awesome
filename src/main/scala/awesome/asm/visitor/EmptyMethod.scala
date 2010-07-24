package awesome
package asm
package visitor

import org.objectweb.asm._
import java.lang.reflect
import jvm.{ Attribute => _, _ }

class EmptyMethod extends MethodVisitor {
  def visitAnnotationDefault(): AnnotationVisitor = null
  def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = null
  def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean): AnnotationVisitor = null
  def visitAttribute(attr: Attribute) { }
  def visitCode() { }
  def visitFrame(jtype: Int, nLocal: Int, local: Array[AnyRef], nStack: Int, stack: Array[AnyRef]) { }
  def visitInsn(opcode: Int) { }
  def visitIntInsn(opcode: Int, operand: Int) { }
  def visitVarInsn(opcode: Int, jvar: Int) { }
  def visitTypeInsn(opcode: Int, jtype: String) { }
  def visitFieldInsn(opcode: Int, owner: String, name: String, desc: String) { }
  def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String) { }
  def visitJumpInsn(opcode: Int, label: Label) { }
  def visitLabel(label: Label) { }
  def visitLdcInsn(cst: AnyRef) { }
  def visitIincInsn(jvar: Int, increment: Int) { }
  def visitTableSwitchInsn(min: Int, max: Int, default: Label, labels: Array[Label]) { }
  def visitLookupSwitchInsn(default: Label, keys: Array[Int], labels: Array[Label]) { }
  def visitMultiANewArrayInsn(desc: String, dims: Int) { }
  def visitTryCatchBlock(start: Label, end: Label, handler: Label, jtype: String) { }
  def visitLocalVariable(name: String, desc: String, signature: String, start: Label, end: Label, index: Int) { }
  def visitLineNumber(line: Int, start: Label) { }
  def visitMaxs(maxStack: Int, maxLocals: Int) { }
  def visitEnd() { }
}