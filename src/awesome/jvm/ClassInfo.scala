package awesome
package jvm

import java.lang.reflect
import attr.{ InnerClasses, Exceptions, ScalaSig, EnclosingMethod }
import validation.Validatable
import pickler.{ ScalaSigParser }

trait ScalaSigAttributed extends Attributed {  
  def scalaSigAttr = findAttr({ case x: ScalaSig => x })
  lazy val scalaSig = scalaSigAttr flatMap (x => ScalaSigParser(x.bytes).toOption)
  def isScala = scalaSigAttr.isDefined
}

case class ClassInfo(
  access: Flags,
  id: Ident,
  superId: Ident,
  interfaces: List[Ident],
  attributes: List[Attribute]
) extends Member with Validatable with ScalaSigAttributed {
  
  type SigType = ClassSignature
  def createSignature = x => ClassSignature(x, name.toScalaString)
  
  lazy val clazz: JClass[_] = name.clazz
  lazy val companion = ClassFileParser(name.toCompanion).get.process
  def clazzName = clazz.getName

  def isScalaObject = isScala && (name endsWith "$")  
  def enclosingMethod = findAttr({ case x: EnclosingMethod => x })
  
  private lazy val jclass = new JavaClass(clazz)
  lazy val reflectMethods = jclass.methods
  lazy val reflectConstructors = jclass.constructors
  lazy val reflectInterfaces = jclass.interfaces
  lazy val reflectEnclosingClass = jclass.enclosingClass
  lazy val reflectEnclosingMethod = jclass.enclosingMethod
  lazy val reflectDeclaringClass = jclass.declaringClass
  
  // def declaringClass: Option[ClassInfo] = {
  //   // if this is primitive None
  //   // if this has no inner class attribute => None
  //   
  // }
  
  /** These methods mimic the java reflection implementation logic.
   *  See doc/jvm.cpp and other files in doc/ for details.
   */
  def enclosingClass: Option[JClass[_]] = {
    // There are five kinds of classes (or interfaces):
    // a) Top level classes
    // b) Nested classes (static member classes)
    // c) Inner classes (non-static member classes)
    // d) Local classes (named classes declared within a method)
    // e) Anonymous classes
    //
    // JVM Spec 4.8.6: A class must have an EnclosingMethod
    // attribute if and only if it is a local class or an
    // anonymous class.    
    reflectEnclosingMethod match {
      // This is a top level or a nested class or an inner class (a, b, or c)
      case None     => reflectDeclaringClass
      // This is a local class or an anonymous class (d or e)
      case Some(m)  => reflectEnclosingClass match {
        case None | Some(`clazz`) => error("Malformed enclosing method information")
        case x                      => x
      }
    }
  }
  
  def simpleName = 
    if (clazz.isArray) clazz.getComponentType.getSimpleName + "[]"
    else simpleBinaryName match {
      case None   => clazzName drop ((clazzName lastIndexOf '.') + 1) // strip package
      // According to JLS3 "Binary Compatibility" (13.1) the binary
      // name of non-package classes (not top level) is the binary
      // name of the immediately enclosing class followed by a '$' followed by:
      // (for nested and inner classes): the simple name.
      // (for local classes): 1 or more digits followed by the simple name.
      // (for anonymous classes): 1 or more digits.

      // Since getSimpleBinaryName() will strip the binary name of
      // the immediatly enclosing class, we are now looking at a
      // string that matches the regular expression "\$[0-9]*"
      // followed by a simple name (considering the simple of an
      // anonymous class to be the empty string).
      
      case Some(str) =>
        if (str == "" || str(0) != '$') error("Malformed class name: " + str)
        // This ends up as the empty string iff this is an anonymous class
        else str.tail dropWhile (ch => '0' <= ch && ch <= '9')
  }
  
  def canonicalName =
    if (clazz.isArray) Option(clazz.getComponentType.getCanonicalName) map (_ + "[]")
    else if (isLocalOrAnonymousClass) None
    else enclosingClass match {
      case None     => clazzName
      case Some(ec) => Option(ec.getCanonicalName) map (_ + "." + simpleName)
    }

  def isTopLevel = enclosingClass.isEmpty
  def isNestedClass = enclosingClass.isDefined

  // These are direct translations of the java implementations.
  def isAnonymousClass        = simpleName == ""
  def isLocalClass            = isLocalOrAnonymousClass && !isAnonymousClass
  def isMemberClass           = simpleBinaryName.isDefined && !isLocalOrAnonymousClass
  
  private def isLocalOrAnonymousClass = enclosingMethod.isDefined
  private def simpleBinaryName        = enclosingClass map (x => clazz.getName drop x.getName.length)

  private def validateEnclosingMethod =
    if (enclosingMethod.isDefined == (isLocalClass || isAnonymousClass)) Nil
    else List("A class must have an EnclosingMethod attribute if and only if it is a local class or an anonymous class.")
    
  lazy val validationFailures: List[String] = List(
    validateEnclosingMethod
  ).flatten
  
  def reflectMethodFor(m: MethodInfo) = jclass findMethodInfo m
  
  private def indent(s: String, num: Int = 8) = (" " * num) + s
  private def indentln(s: String) = indent(s) + "\n"
  
  def whatString = {
    def adj(cond: Boolean, word: String): String = if (cond) word else ""
    val noun = 
      if (access.isInterface)
        if (isScala) "trait" else "interface"
      else
        if (isScalaObject) "object" else "class"
    
    spaceSepString(
      adj(isAnonymousClass, "anonymous"),
      adj(isLocalClass, "local"),
      adj(isMemberClass, "inner"),
      noun
    )
  }
  
  def nameString    = javaTypeToString(clazz)
  def extendsStr    = clazz.getGenericSuperclass match { 
    case null => ""
    case x    => indent("extends " + javaTypeToString(x), 5) + "\n"
  }
  def interfaceStr  = reflectInterfaces map (x => indent("with " + javaTypeToString(x))) mkString "\n"
  
  override def toString = spaceSepString(
    accessString,
    whatString,
    nameString + "\n" + extendsStr + interfaceStr
  )
}
