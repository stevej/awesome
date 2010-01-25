package awesome
package jvm
package attr

import scala.util.parsing.combinator._
import awesome.parser._
import ClassFileParser.{
  attribute_info, field_attribute_info, method_attribute_info,
  class_attribute_info, code_attribute_info, inner_class_info
}

class AttributeParser(cf: ParsedClassFile) extends JVMParsers 
        with (attribute_info => Attribute) {
  import cf._
  import cf.pool._
  
  lazy val stringAtIndex: Parser[String] = u2 ^^ (x => asString(x))
  lazy val classAtIndex: Parser[Ident] = u2 ^^ (x => asClass(x))
  lazy val constantAtIndex: Parser[Any] = u2 ^^ (x => asConstant(x))
  lazy val fieldDescriptorAtIndex = u2 ^^ (x => asFieldDescriptor(x, ""))
  lazy val fieldSignatureAtIndex = stringAtIndex ^^ (x => FieldSignature(x))
  
  def unlessZero[T](f: Int => T): Parser[Option[T]] = u2 ^^ {
    case 0  => None
    case x  => Some(f(x))
  }
  
  lazy val optStringAtIndex = unlessZero(asString)
  lazy val optClassAtIndex = unlessZero(asClass)
  lazy val optMethodDescriptorAtIndex = unlessZero(asNameAndMethodDescriptor)
  
  val max_stack, max_locals, access_flags = u2
  
  def exception_table(size: Int) = {
    // If the value of the catch_type item is zero, this exception handler is called for all exceptions.
    val catch_type = optClassAtIndex
    val start_pc, end_pc, handler_pc = u2
    val exception = start_pc ~ end_pc ~ handler_pc ~ catch_type ^^ ExceptionTable
    
    repN(size, exception)
  }
  
  lazy val code_attribute = u2 ~ (u4 >> bytes) ^^ { case a~b => apply(code_attribute_info(a, b)) }
  
  // "Code"
  lazy val code = (
    max_stack ~ 
    max_locals ~
    (u4 >> bytes) ~
    (u2 >> exception_table) ~
    collect(code_attribute)
  )
  
  lazy val innerClassInfo = u2 ~ u2 ~ u2 ~ access_flags ^^ inner_class_info
  lazy val innerClass = innerClassInfo ^^ {
    case info @ inner_class_info(inner_class, outer_class, inner_name, access_flags) =>
      def notZero[T](x: Int, f: Int => T): Option[T] = if (x == 0) None else Some(f(x))        
      val innerResult = notZero(inner_class, asClass)
      
      // all the inner class indices can be zero
      InnerClass(
        info, 
        // The value of the inner_class_info_index item must be zero or a valid index into the constant_pool table.
        innerResult,
        // If C is not a member, the value of the outer_class_info_index item must be zero.
        notZero(outer_class, asClass),
        // If C is anonymous, the value of the inner_name_index item must be zero.
        // (But in real life it might be the empty string.)
        notZero(inner_name, asString) flatMap nonEmptyString map Ident, 
        InnerClassFlags(access_flags, innerResult getOrElse Ident(""))
      )
  }
  
  // If the current class is not immediately enclosed by a method or constructor,
  // then the value of the method_index item must be zero.
  lazy val enclosingMethod = classAtIndex ~ optMethodDescriptorAtIndex
  
  lazy val annotation: Parser[Annotation] =
    fieldDescriptorAtIndex ~ collect(elementValuePair) ^^ Annotation
    
  lazy val elementValuePair: Parser[ElementValuePair] =
    stringAtIndex ~ elementValue ^^ ElementValuePair
  
  lazy val elementValue: Parser[Any] = u1 >> {
    case 'B'  => u2 ^^ asByte
    case 'S'  => u2 ^^ asShort
    case 'C'  => u2 ^^ asChar
    case 'I'  => u2 ^^ asInt
    case 'J'  => u2 ^^ asLong
    case 'F'  => u2 ^^ asFloat
    case 'D'  => u2 ^^ asDouble
    case 'Z'  => u2 ^^ asBoolean
    case 's'  => u2 ^^ asStringConst
    case 'e'  => stringAtIndex ~ stringAtIndex ^^ tuplify
    case 'c'  => fieldDescriptorAtIndex
    case '@'  => annotation
    case '['  => collect(elementValue)
  }

  private def local_variable_common[T](p: Parser[T]) = u2 ~ u2 ~ stringAtIndex ~ p ~ u2    
  lazy val local_variable       = local_variable_common(fieldDescriptorAtIndex) ^^ LocalVariable
  lazy val local_variable_type  = local_variable_common(fieldSignatureAtIndex)  ^^ LocalVariableType
  lazy val parameterAnnotations = collect_u1(collect(annotation))
  
  val knownAttributes: String =>? Parser[Attribute] = {
    case "AnnotationDefault"                    => elementValue                   ^^ AnnotationDefault
    case "Code"                                 => code                           ^^ Code
    case "ConstantValue"                        => constantAtIndex                ^^ (x => ConstantValue(x))
    case "Deprecated"                           => success(Deprecated)
    case "EnclosingMethod"                      => enclosingMethod                ^^ EnclosingMethod
    case "Exceptions"                           => collect(classAtIndex)          ^^ Exceptions
    case "InnerClasses"                         => collect(innerClass)            ^^ InnerClasses
    case "LineNumberTable"                      => collect(u2 ~ u2 ^^ LineNumber) ^^ LineNumberTable
    case "LocalVariableTable"                   => collect(local_variable)        ^^ LocalVariableTable
    case "LocalVariableTypeTable"               => collect(local_variable_type)   ^^ LocalVariableTypeTable
    case "RuntimeInvisibleAnnotations"          => collect(annotation)            ^^ RuntimeInvisibleAnnotations
    case "RuntimeInvisibleParameterAnnotations" => parameterAnnotations           ^^ RuntimeInvisibleParameterAnnotations
    case "RuntimeVisibleAnnotations"            => collect(annotation)            ^^ RuntimeVisibleAnnotations
    case "RuntimeVisibleParameterAnnotations"   => parameterAnnotations           ^^ RuntimeVisibleParameterAnnotations
    case "ScalaSig"                             => drain                          ^^ ScalaSig
    case "Signature"                            => stringAtIndex                  ^^ (SignatureAttr(_, None))
    case "SourceDebugExtension"                 => drain          ^^ toUTF8String ^^ SourceDebugExtension
    case "SourceFile"                           => stringAtIndex                  ^^ SourceFile
    // case "StackMapTable"                        => success(StackMapTable())
    case "Synthetic"                            => success(Synthetic)
  }
  def unknown(name: String) = drain ^^ (xs => Unknown(name, xs))
  
  def apply(attr: attribute_info): Attribute = apply(attr, "%s")
  def apply(attr: attribute_info, owner: Ident): Attribute = {
    val attrName = asString(attr.name_index)
    val reader = new ByteReader(attr.info)    
    
    def runParse(p: Parser[Attribute]) = phrase(p)(reader) match {
      case Success(attr, _)           => attr
      case x: NoSuccess               => Malformed(attrName, x.msg)
    }
    
    runParse(
      if (attrName == "Signature") stringAtIndex ^^ (x => SignatureAttr(x, Some(owner)))
      else if (knownAttributes isDefinedAt attrName) knownAttributes(attrName)
      else unknown(attrName)
    )
  }
}