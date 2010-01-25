package awesome
package jvm

/** Generic signatures. */  
trait Signature extends Descriptor {
  def text: String
  def parsed: Sig.AnySignature
  def decoded: String = parsed.toString
  def name: String = "%s"
  override def isSignature: Boolean = true
  
  override def toString = decoded format name
}

class FieldSignature(val text: String) extends Signature {
  lazy val parsed: Sig.FieldSignature = SignatureParser.field(text)
}
class MethodSignature(val text: String) extends Signature { 
  lazy val parsed: Sig.MethodTypeSignature = SignatureParser.method(text)
}
class ClassSignature(val text: String) extends Signature {
  lazy val parsed: Sig.ClassSignature = SignatureParser.jclass(text)
}

object FieldSignature {
  def apply(text: String, _name: String): FieldSignature = new FieldSignature(text) { override def name = _name }
  def apply(text: String): FieldSignature = new FieldSignature(text)
}
object MethodSignature {
  def apply(text: String, _name: String): MethodSignature = new MethodSignature(text) { override def name = _name }
  def apply(text: String): MethodSignature = new MethodSignature(text)
}
object ClassSignature {
  def apply(text: String, _name: String): ClassSignature = new ClassSignature(text) { override def name = _name }
  def apply(text: String): ClassSignature = new ClassSignature(text)
}
