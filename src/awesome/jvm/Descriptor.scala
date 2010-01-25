package awesome
package jvm

import java.lang.reflect
import types._
import Descriptor._

trait Descriptor { 
  def text: String 
  def decoded: String
  def name: String
  def isSignature: Boolean = false

  // def name: Option[String]

  override def toString() = decoded  
}

case class FieldDescriptor(text: String, name: String) extends Descriptor { 
  def this(text: String) = this(text, "")
  def decoded = getType(text).toString

  def getType(buf: String = text): Type =
    if (Base.anyval isDefinedAt buf.head) Base.anyval(buf.head)
    else {
      val (dims, rest) = buf span (_ == '[')
      if (dims.size == 0) ObjectType(rest drop 1 takeWhile (_ != ';'))
      else ArrayType(dims.size, getType(rest))
    }
}
case class MethodDescriptor(text: String, name: String) extends Descriptor {
  def this(text: String) = this(text, "")
  lazy val MethodType(argTypes, returnType) =
    DescriptorParser method text getOrElse error("Descriptor parse error: " + text)
    
  def decoded = "(%s) => %s".format(argTypes mkString ", ", returnType)
}

object Descriptor {  
  def getDescriptor(c: JClass[_]): String = (Base.forClass get c) match {
    case Some(x)        => x.tag.toString
    case _ if c.isArray => "[" + getDescriptor(c.getComponentType)
    case _              => "L%s;".format(c.getName.toInternal)
  }
  
  def getMethodDescriptor(m: reflect.Method) =
    "(%s)%s".format(m.getParameterTypes map getDescriptor mkString, getDescriptor(m.getReturnType))

  def getConstructorDescriptor(c: reflect.Constructor[_]) =
    "(%s)V".format(c.getParameterTypes map getDescriptor mkString)
  
  def fromString(s: String) = {
    require(s != null)
    if (s startsWith "(") new MethodDescriptor(s)
    else new FieldDescriptor(s)
  }
    
  implicit def fromFunction(x: AnyRef) = getDescriptorFromFunction(x)
  implicit def fromJavaMethod(x: reflect.Method) = MethodDescriptor(getMethodDescriptor(x), x.getName)
  implicit def fromJavaConstructor(x: reflect.Constructor[_]) = MethodDescriptor(getConstructorDescriptor(x), x.getName)
  implicit def fromJavaField(x: reflect.Field) = FieldDescriptor(getDescriptor(x.getType), x.getName)
 
  def applyMethods(x: AnyRef) =
    x.getClass.getMethods.toList filter (_.getName == "apply")
  
  def getDescriptorFromFunction(x: AnyRef) =
    fromJavaMethod(applyMethods(x) find (x => !x.isBridge) get)
}
