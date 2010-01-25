package awesome
package asm
package visitor

import org.objectweb.asm._

class EmptyField extends FieldVisitor {
  def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = null
  def visitAttribute(attr: Attribute) { }
  def visitEnd() { }
}
