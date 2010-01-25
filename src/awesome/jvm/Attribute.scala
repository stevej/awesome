package awesome
package jvm

// http://java.sun.com/docs/books/jvms/second_edition/jvms-clarify.html

trait Attribute {
  def name: String
  def isUnknown = false
  def isAnnotation = false
  
  def isInnerClasses = name == "InnerClasses"
  def isSynthetic = name == "Synthetic"
  def isDeprecated = name == "Deprecated"
  def isSignature = name == "Signature"
  def isConstantValue = name == "ConstantValue"
  def isEnclosingMethod = name == "EnclosingMethod"
}

object Attribute {
  def unapply(other: Any) = other match {
    case x: Attribute => Some((x.name))
    case _            => None
  }
}
