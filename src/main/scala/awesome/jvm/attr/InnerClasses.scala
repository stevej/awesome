package awesome
package jvm
package attr

import ClassFileParser.inner_class_info

// Every CONSTANT_Class_info entry in the constant_pool table which represents a class or interface C 
// that is not a package member must have exactly one corresponding entry in the classes array.
// 
// If a class has members that are classes or interfaces, its constant_pool table
// (and hence its InnerClasses attribute) must refer to each such member, even if that member
// is not otherwise mentioned by the class. These rules imply that a nested class or interface
// member will have InnerClasses information for each enclosing class and for each immediate member.
case class InnerClass(
  attr_info: inner_class_info,
  innerClass: Option[Ident],
  outerClass: Option[Ident],
  innerName: Option[Ident],
  access: InnerClassFlags
) extends Member {
  private lazy val inner_class_info(inner_class_index, outer_class_index, inner_name_index, _) = attr_info
  lazy val innerNameStr = innerName map (_.name) getOrElse "<inner_class_info_index=0>"
  lazy val outerNameStr = outerClass map (_.name) getOrElse "<outer_class_info_index=0>"
  lazy val fullNameStr = innerClass map (_.name) getOrElse "<inner_index=0>"
  
  // Member compliance
  def id = innerClass getOrElse Ident("<anon inner class>")
  def attributes = Nil
  type SigType = Nothing
  def createSignature = _ => error("Inner classes do not have signatures")

  override lazy val sig = None
  override def unqualifiedName = innerName map (_.toUnqualified) getOrElse "<anon>"
  override def isDeprecated = false
  override def isSynthetic = access.isSynthetic

  // http://blogs.sun.com/darcy/entry/nested_inner_member_and_top
  //
  // One way declared types in Java differ from one another is whether the type is a class (which
  // includes enums) or an interface (which includes annotation types). An independent property of a
  // type is its relation to the surrounding lexical context. A top level class does not appear
  // inside another class or interface. If a type is not top level it is nested. However, there are
  // a number of distinct ways a type can be nested. First, a type can be a member of another type;
  // a member type is directly enclosed by another type declaration. All member types have names;
  // however, some member types are inner classes and others are not. If a type is explicitly or
  // implicitly static, it is not an inner class; all member interfaces are implicitly static.
  // 
  // Inner classes also include local classes, which are named classes declared inside of a block
  // like a method or constructor body, and anonymous classes, which are unnamed classes whose
  // instances are created in expressions and statements. Anonymous classes are used to implement
  // specialized enum constants. Inner classes have access to instance variables of any lexically
  // enclosing instance; that is, the fields of the object an inner class is created in reference
  // to. However, not all inner classes have enclosing instances; inner classes in static contexts,
  // like an anonymous class used in a static initializer block, do not.
  
  def toInternal = innerClass map (_.toInternal) getOrElse ""
  def isMemberClass = outerClass.isDefined
  def isAnonymous = innerName.isEmpty
  def toClassFile = ClassFileParser(toInternal).get.process

  def toJavap = {
    spaceSepString(
      access.toString,
      "#%d= #%d of %d;".format(inner_name_index, inner_class_index, outer_class_index),
      "//%s=class %s of class %s".format(innerNameStr, fullNameStr, outerNameStr)
    )
  }
  def toScala = {
    spaceSepString(
      access.toString,
      "class",
      innerNameStr.toDecoded
    )
  }

  override def toString = toJavap
}
case class InnerClasses(
  classes: List[InnerClass]
) extends AttributeClass("InnerClasses") { 
  
  override def toString = classes.mkString("  InnerClass:\n   ", "\n   ", "")
}
