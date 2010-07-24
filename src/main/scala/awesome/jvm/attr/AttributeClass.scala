package awesome
package jvm
package attr

// trait FieldAttribute extends Attribute
// trait MethodAttribute extends Attribute
// trait ClassAttribute extends Attribute
// trait CodeAttribute extends Attribute

abstract class AttributeClass(val name: String) extends Attribute
 
case class Unknown(name: String, bytes: Seq[Byte]) extends Attribute {
  override val isUnknown = true
}
case class Malformed(name: String, errorMessage: String) extends Attribute

// 4.7.1
// Attributes defined other than by Sun must have names chosen according to the package
// naming convention defined by The Java Language Specification. For instance, a new attribute
// defined by Netscape might have the name "com.Netscape.new-attribute".
//
case class ScalaSig(bytes: Seq[Byte]) extends AttributeClass("ScalaSig") {
  override def toString = "ScalaSig(%d bytes)".format(bytes.size)
}

// 4.7.2
// A ConstantValue attribute represents the value of a constant field.
// If the ACC_STATIC bit is set then the field is assigned the ConstantValue
// as part of the initialization of the declaring class or interface.
// If a field_info structure representing a non-static field has a ConstantValue
// attribute, then that attribute must silently be ignored.  
case class ConstantValue[T](value: T) extends AttributeClass("ConstantValue") {
  override def toString = value.toString
}

// 4.7.3
// See Code.scala
// 4.7.4
case class StackMapTable() extends AttributeClass("StackMapTable") { }

// 4.7.5
case class Exceptions(exceptions: List[Ident]) extends AttributeClass("Exceptions") { }

// 4.7.6
// See InnerClasses.scala

// 4.7.7
// A class must have an EnclosingMethod attribute if and only if
// it is a local class or an anonymous class.
case class EnclosingMethod(
  classId: Ident,
  descriptor: Option[MethodDescriptor]
) extends AttributeClass("EnclosingMethod") { 
  
  def methodName = descriptor map (_.name)
}

// 4.7.8
// A class member that does not appear in the source code must be marked using a
// Synthetic attribute, or else it must have its ACC_SYNTHETIC bit set. The only
// exceptions to this requirement are for default constructors and the class
// initialization method.
case object Synthetic extends AttributeClass("Synthetic") { }

// 4.7.9
case class SignatureAttr(text: String, owner: Option[Ident]) extends AttributeClass("Signature") {
  lazy val formatString = SignatureParser(text).toString.toScalaString
  lazy val ownerString = owner map (_.toString) getOrElse "%s"
  
  override def toString = formatString format ownerString
}
// 4.7.10
case class SourceFile(sourceFile: String) extends AttributeClass("SourceFile") { }

// 4.7.11
case class SourceDebugExtension(debug: String) extends AttributeClass("SourceDebugExtension") { }

// 4.7.12
case class LineNumber(startPC: Int, line: Int) {
  override def toString = "line %d: %d".format(startPC, line)
}
case class LineNumberTable(lineNumbers: List[LineNumber]) extends AttributeClass("LineNumberTable") {  
  override def toString = lineNumbers.mkString("  LineNumberTable:\n   ", "\n   ", "")
}

// shared by 13 and 14
trait LocalVariableBase[T] {
  def startPC: Int
  def length: Int
  def name: String
  def value: T
  def index: Int
}

trait LocalVariableTableBase[T, U <: LocalVariableBase[T]] {
  def locals: List[U]
  
  lazy val names: Map[Int, String] = Map((locals map (x => x.index -> x.name)) : _*)
  lazy val types: Map[Int, T] = Map((locals map (x => x.index -> x.value)) : _*)
  lazy val typesText: Map[Int, String] = types map { case (k, v) => (k, v.toString) }
  // 
  // def apply(index: Int): String = nameMap(index)
  // def get(index: Int): Option[String] = nameMap get index
  // def isDefinedAt(index: Int) = nameMap isDefinedAt index
}

// 4.7.13
case class LocalVariable(
  startPC: Int,
  length: Int,
  name: String,
  value: Descriptor,
  index: Int
) extends LocalVariableBase[Descriptor] { 
  
  def descriptor = value
}
case class LocalVariableTable(locals: List[LocalVariable])
        extends AttributeClass("LocalVariableTable")
        with LocalVariableTableBase[Descriptor, LocalVariable] {

  //
}

// 4.7.14
// The LocalVariableTypeTable	attribute	differs	from the LocalVariableTable attribute
// in that it provides signature information rather than descriptor information.
// This difference is only significant for variables whose type is a generic reference type.
// Such variables will appear in both tables, while variables of other types will appear
// only in LocalVariableTable.
//
case class LocalVariableType(
  startPC: Int,
  length: Int,
  name: String,
  value: Signature,
  index: Int
) extends LocalVariableBase[Signature] {

  def signature = value
}
case class LocalVariableTypeTable(locals: List[LocalVariableType])
        extends AttributeClass("LocalVariableTypeTable")
        with LocalVariableTableBase[Signature, LocalVariableType] {
  //
}

// 4.7.15
case object Deprecated extends AttributeClass("Deprecated") { }

// Shared by annotation attributes
case class ElementValuePair(key: String, value: Any) { 
  override def toString = key + "=" + value
}
case class Annotation(descriptor: FieldDescriptor, pairs: List[ElementValuePair]) {
  override def toString = "@%s%s".format(
    descriptor, if (pairs.isEmpty) "" else pairs.mkString("(", ", ", ")")
  )
}

trait RuntimeAnnotation extends Attribute {
  override def isAnnotation = true
  def isVisible: Boolean
  def isParameter: Boolean
}

// 4.7.16
case class RuntimeVisibleAnnotations(annotations: List[Annotation])
        extends AttributeClass("RuntimeVisibleAnnotations")
        with RuntimeAnnotation {

  def isVisible = true
  def isParameter = false
  override def toString = annotations mkString ", " 
}

// 4.7.17
case class RuntimeInvisibleAnnotations(annotations: List[Annotation])
        extends AttributeClass("RuntimeInvisibleAnnotations") 
        with RuntimeAnnotation {
       
  def isVisible = false
  def isParameter = false          
  override def toString = annotations mkString ", "  
}

// 4.7.18
case class RuntimeVisibleParameterAnnotations(annotations: List[List[Annotation]])
        extends AttributeClass("RuntimeVisibleParameterAnnotations")
        with RuntimeAnnotation {

  def isVisible = true
  def isParameter = true
  private def annotationStr(xs: List[Annotation]) = if (xs.isEmpty) "" else xs mkString " "
  override def toString =
    ((1 to annotations.size), annotations).zipped map ((p, a) => spaceSepString("p" + p, annotationStr(a))) mkString ("Visible(", ", ", ")")
}

// 4.7.19
case class RuntimeInvisibleParameterAnnotations(annotations: List[List[Annotation]])
        extends AttributeClass("RuntimeInvisibleParameterAnnotations")
        with RuntimeAnnotation {
  
  def isVisible = false
  def isParameter = true
  override def toString = annotations.mkString("Invisible(", ", ", ")")
}

// 4.7.20
case class AnnotationDefault(value: Any) extends AttributeClass("AnnotationDefault") { }