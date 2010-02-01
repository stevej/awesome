package awesome
package jvm

import scala.util.parsing.combinator._
import scala.annotation.tailrec
import io.ByteCode
import parser._
import ClassFileParser._

case class ParsedPool(pool: constant_pool) {
  def referencedClasses = pool.infos partialMap { case cp_class(x)  => asString(x) }
  def referencedPackages = referencedClasses uniqmap (_.toPackage)

  def formatError(index: Int) =
    error("parse error at index '%d': unexpected '%s'".format(index, pool(index)))
  
  private def asType[T](pf: cp_info =>? T): Int => T =
    (index: Int) =>
      if (pf.isDefinedAt(pool(index))) pf(pool(index))
      else formatError(index)

  val asClass       = asType { case cp_class(x) => Ident(asString(x)) }
  val asString      = asType { case cp_utf8(str) => str }
  val asConstant    = asType { case x: cp_value => x.value }
  val asNameAndType = asType { case cp_name_and_type_ref(indexPair) => indexPair }
  val asBoolean     = asType { case cp_int(x) => x != 0 }
  val asChar        = asType { case cp_int(x) => x.toChar }
  val asByte        = asType { case cp_int(x) => x.toByte }
  val asShort       = asType { case cp_int(x) => x.toShort }
  val asInt         = asType { case cp_int(x) => x }
  val asLong        = asType { case cp_long(x) => x }
  val asFloat       = asType { case cp_float(x) => x }
  val asDouble      = asType { case cp_double(x) => x }
  val asStringConst = asType { case cp_value(x: String) => x }
  
  def asIdent(x: named_info)  = Ident(asString(x.name_index))
  def asName(index: Int)      = Ident(asString(asNameAndType(index)._1))

  def asFieldDescriptor(index: Int, fieldName: String) =
    FieldDescriptor(asString(index), fieldName)

  def asMethodDescriptor(index: Int, methodName: String) =
    MethodDescriptor(asString(index), methodName)

  def asNameAndMethodDescriptor(index: Int) = {
    val (nameIndex, typeIndex) = asNameAndType(index)
    MethodDescriptor(asString(typeIndex), asString(nameIndex))
  }
  def asNameAndFieldDescriptor(index: Int) = {
    val (nameIndex, typeIndex) = asNameAndType(index)
    FieldDescriptor(asString(typeIndex), asString(nameIndex))
  }
}

class ParsedClassFile private[jvm] (
  val version: (Int, Int),
  val pool: ParsedPool,
  val access_flags: Int,
  val this_class: Int,
  val super_class: Int,
  val interfaces: List[Int],
  val fields: List[field_info],
  val methods: List[method_info],
  val attributes: List[class_attribute_info]
) {
  
  import pool._

  override def toString = {
    List(
      "%s extends %s".format(thisClass, superId),
      "Constant pool:",
      methods map (_.toString) mkString "\n",
      fields map (_.toString) mkString "\n"
    ).flatten mkString
  }
  
  def thisClass = asClass(this_class)
  def superId: Ident =
    if (super_class == 0) Ident(JL_OBJECT)
    else asClass(super_class)
  
  lazy val classInfo = ClassInfo(
    ClassFlags(access_flags, thisClass),
    thisClass,
    superId,
    interfaces map asClass,
    attributes map (x => attributeParser(x, thisClass))
  )
  def asFieldInfo(x: field_info) = {
    val id = asIdent(x)
    FieldInfo(
      classInfo,
      FieldFlags(x.access_flags, id.name),
      id,
      asFieldDescriptor(x.descriptor_index, id.name),
      x.attributes map (x => attributeParser(x, id))
    )
  }
  def asMethodInfo(x: method_info) = {
    val id = asIdent(x)
    MethodInfo(
      classInfo,
      MethodFlags(x.access_flags, id.name),
      id,
      asMethodDescriptor(x.descriptor_index, id.name),
      x.attributes map (x => attributeParser(x, id))
    )
  }
  
  lazy val attributeParser = new attr.AttributeParser(this)
  
  def hasAttribute(attr: String) = attributes exists {
    case class_attribute_info(index, _) if pool.asString(index) == attr => true
    case _                                                              => false
  }
  
  def process() = ClassFile(pool, classInfo, fields map asFieldInfo, methods map asMethodInfo)
}