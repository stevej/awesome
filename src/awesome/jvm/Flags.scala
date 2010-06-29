package awesome
package jvm

object Flags {
  final val ACC_PUBLIC = 0x0001         // class, field, method
  final val ACC_PRIVATE = 0x0002        // class, field, method
  final val ACC_PROTECTED = 0x0004      // class, field, method
  final val ACC_STATIC = 0x0008         // field, method
  final val ACC_FINAL = 0x0010          // class, field, method
  final val ACC_SUPER = 0x0020          // class
  final val ACC_SYNCHRONIZED = 0x0020   // method
  final val ACC_VOLATILE = 0x0040       // field
  final val ACC_BRIDGE = 0x0040         // method
  final val ACC_VARARGS = 0x0080        // method
  final val ACC_TRANSIENT = 0x0080      // field
  final val ACC_NATIVE = 0x0100         // method
  final val ACC_INTERFACE = 0x0200      // class
  final val ACC_ABSTRACT = 0x0400       // class, method
  final val ACC_STRICT = 0x0800         // method
  final val ACC_SYNTHETIC = 0x1000      // class, field, method
  final val ACC_ANNOTATION = 0x2000     // class
  final val ACC_ENUM = 0x4000           // class(?) field inner
  
  /** Some convenient groupings */
  final val accessFlags = List(ACC_PUBLIC, ACC_PROTECTED, ACC_PRIVATE)
  final val compilerFlags = List(ACC_SUPER, ACC_BRIDGE, ACC_VARARGS, ACC_SYNTHETIC)
  final val annotationFlags = List(ACC_VOLATILE, ACC_TRANSIENT, ACC_NATIVE)
  final val allFlags = (
    accessFlags ::: compilerFlags ::: annotationFlags :::
    List(ACC_STATIC, ACC_FINAL, ACC_SYNCHRONIZED, ACC_INTERFACE, ACC_ABSTRACT, ACC_STRICT, ACC_ANNOTATION, ACC_ENUM)
  ).distinct
  
  /** Some flags are overloaded */
  def name(flag: Int, isMethod: Boolean) = flag match {
    case ACC_PUBLIC       => "ACC_PUBLIC"
    case ACC_PRIVATE      => "ACC_PRIVATE"
    case ACC_PROTECTED    => "ACC_PROTECTED"
    case ACC_STATIC       => "ACC_STATIC"
    case ACC_FINAL        => "ACC_FINAL"
    case ACC_SUPER        => if (isMethod) "ACC_SYNCHRONIZED" else "ACC_SUPER"
    case ACC_VOLATILE     => if (isMethod) "ACC_BRIDGE" else "ACC_VOLATILE"
    case ACC_TRANSIENT    => if (isMethod) "ACC_VARARGS" else "ACC_TRANSIENT"
    case ACC_NATIVE       => "ACC_NATIVE"
    case ACC_INTERFACE    => "ACC_INTERFACE"
    case ACC_ABSTRACT     => "ACC_ABSTRACT"
    case ACC_STRICT       => "ACC_STRICT"
    case ACC_SYNTHETIC    => "ACC_SYNTHETIC"
    case ACC_ANNOTATION   => "ACC_ANNOTATION"
    case ACC_ENUM         => "ACC_ENUM"
  }
  
  // def forField(flags: Int, isInterface: Boolean): Flags =
  //   if (isInterface) InterfaceFieldFlags(flags) else FieldFlags(flags)
  //   
  // def forMethod(flags: Int, isInterface: Boolean): Flags =
  //   if (isInterface) InterfaceMethodFlags(flags) else MethodFlags(flags)
  // 
  // def forClass(flags: Int): Flags =
  //   if ((flags & ACC_INTERFACE) != 0) InterfaceFlags(flags) else ClassFlags(flags)
  // 
  // def forConstructor(flags: Int): Flags = ConstructorFlags(flags)
}
import Flags._

sealed trait Flags {
  def flags: Int
  def owner: Ident = ""
  def pkg: String = if (owner == "") "???" else owner.toPackage
  def containerType: String
  def requiredFlags: List[Int]
  def optionalFlags: List[Int]
  def excludedFlags: List[Int] =
    allFlags filterNot (requiredFlags ::: optionalFlags contains _)
  
  def isMethod = false      // for disambiguating overloaded flags
  def name(flag: Int): String = Flags.name(flag, isMethod)
  
  def flagImpliesFlags: List[(Int, Int)] = Nil
  def flagExcludesFlags: List[(Int, Int)] = Nil
  def otherConditions: List[(Boolean, String)] = List(
    (accessFlagsCount > 1, "At most one of ACC_PUBLIC, ACC_PROTECTED, and ACC_PRIVATE should be set")
  )
  
  private def requireFailure(flag: Int) =
    containerType + " requires " + name(flag)
  private def zeroFailure(flag: Int) =
    containerType + " must not set " + name(flag)
    
  private def impliesFailure(flag1: Int, flag2: Int) =
    "In %s, %s requires %s".format(containerType, name(flag1), name(flag2))
  private def excludesFailure(flag1: Int, flag2: Int) =
    "In %s, %s is mutally exclusive with %s".format(containerType, name(flag1), name(flag2))
    
  def validate = validationFailures.isEmpty
  lazy val validationFailures: List[String] = {
    List(
      requiredFlags filterNot (this hasFlag _) map requireFailure,
      excludedFlags filter (this hasFlag _) map zeroFailure,
      flagImpliesFlags collect { case (k, v) if hasFlag(k) && !hasFlag(v) => impliesFailure(k, v) },
      flagExcludesFlags collect { case (k, v) if hasFlag(k) && hasFlag(v) => excludesFailure(k, v) },
      otherConditions collect { case (true, msg) => msg }
    ).flatten
  }

  def hasFlag(f: Int) = (flags & f) != 0

  def isAbstract = this hasFlag ACC_ABSTRACT
  def isAnnotation = this hasFlag ACC_ANNOTATION
  def isEnum = this hasFlag ACC_ENUM
  def isFinal = this hasFlag ACC_FINAL
  def isInterface = this hasFlag ACC_INTERFACE
  def isNative = this hasFlag ACC_NATIVE
  def isStatic = this hasFlag ACC_STATIC
  def isStrict = this hasFlag ACC_STRICT
  def isSynthetic = this hasFlag ACC_SYNTHETIC
  
  /** Overloaded flags verify what kind of construct these flags are from */
  def isBridge        =  isMethod && (this hasFlag ACC_BRIDGE)
  def isSuper         = !isMethod && (this hasFlag ACC_SUPER)
  def isSynchronized  =  isMethod && (this hasFlag ACC_SYNCHRONIZED)
  def isTransient     = !isMethod && (this hasFlag ACC_TRANSIENT)
  def isVarArgs       =  isMethod && (this hasFlag ACC_VARARGS)
  def isVolatile      = !isMethod && (this hasFlag ACC_VOLATILE)  
  
  /** Access flags */
  def isPublic = this hasFlag ACC_PUBLIC
  def isProtected = this hasFlag ACC_PROTECTED
  def isPrivate = this hasFlag ACC_PRIVATE
  def isPackagePrivate = !isPublic && !isProtected && !isPrivate
  
  def accessFlagsCount = List(isPublic, isProtected, isPrivate) filter (_ == true) size
  
  def accessString =
    if (isPublic) "public"
    else if (isProtected) "protected"
    else if (isPrivate) "private"
    else "private[%s]".format(
      if (pkg == "") "package" else pkg
    )
  
  private def scalaNameStr(fs: List[Int]) = spaceSepString(fs filter hasFlag map scalaName: _*)
  private def annotationStr = scalaNameStr(annotationFlags)  
  private def otherStr = scalaNameStr(allFlags filterNot (accessFlags ::: annotationFlags contains _))
  
  def scalaName(flag: Int) = flag match {
    case ACC_PUBLIC       => "public"       // redundant, but we want to match javp
    case ACC_PRIVATE      => "private"
    case ACC_PROTECTED    => "protected"
    case ACC_STATIC       => "" // "<static>"
    case ACC_FINAL        => "final"
    case ACC_SUPER        => ""
    case ACC_VOLATILE     => "@volatile"
    case ACC_TRANSIENT    => "@transient"
    case ACC_NATIVE       => "@native"
    case ACC_INTERFACE    => ""   // "interface"
    case ACC_ABSTRACT     => "abstract"
    case ACC_STRICT       => "<strictfp>"   // should be @strictfp but unimplemented
    case ACC_SYNTHETIC    => "<synthetic>"
    case ACC_ANNOTATION   => "<annotation>"
    case ACC_ENUM         => "<enum>"
  }
  
  override def toString = spaceSepString(annotationStr, accessString, otherStr)
}

trait MethodFlagNames extends Flags {
  override def isMethod = true
  override def scalaName(flag: Int) = flag match {
    case ACC_SYNCHRONIZED => "synchronized"
    case ACC_BRIDGE       => "<bridge>"
    case ACC_VARARGS      => "" // "<varargs>"
    case _                => super.scalaName(flag)
  }
}

case class ClassFlags(flags: Int, override val owner: Ident) extends Flags {
  def containerType = "class"
  def requiredFlags = List(ACC_SUPER)
  def optionalFlags = List(ACC_PUBLIC, ACC_SYNTHETIC, ACC_FINAL, ACC_ABSTRACT, ACC_ENUM)
  override def flagExcludesFlags = List((ACC_ABSTRACT, ACC_FINAL))  
}

case class InnerClassFlags(flags: Int, override val owner: Ident) extends Flags {
  def containerType = "inner class"
  def requiredFlags = Nil
  def optionalFlags = accessFlags ::: List(ACC_STATIC, ACC_FINAL, ACC_INTERFACE, ACC_ABSTRACT, ACC_SYNTHETIC, ACC_ANNOTATION, ACC_ENUM)  
}

case class InterfaceFlags(flags: Int) extends Flags {
  def containerType = "interface"
  def requiredFlags = List(ACC_INTERFACE, ACC_ABSTRACT)
  def optionalFlags = List(ACC_PUBLIC, ACC_SYNTHETIC, ACC_ANNOTATION)
}

case class FieldFlags(flags: Int, override val owner: Ident) extends Flags {
  def containerType = "class field"
  def requiredFlags = Nil
  def optionalFlags = accessFlags ::: List(ACC_STATIC, ACC_FINAL, ACC_VOLATILE, ACC_TRANSIENT, ACC_SYNTHETIC, ACC_ENUM)
  override def flagExcludesFlags = List((ACC_FINAL, ACC_VOLATILE))  
}

case class InterfaceFieldFlags(flags: Int) extends Flags {
  def containerType = "interface field"
  def requiredFlags = List(ACC_PUBLIC, ACC_STATIC, ACC_FINAL)
  def optionalFlags = List(ACC_SYNTHETIC)
} 

case class MethodFlags(flags: Int, override val owner: Ident) extends Flags with MethodFlagNames {
  def containerType = "class method"
  def requiredFlags = Nil
  def optionalFlags = accessFlags ::: List(ACC_STATIC, ACC_FINAL, ACC_SYNCHRONIZED) :::
    List(ACC_BRIDGE, ACC_VARARGS, ACC_NATIVE, ACC_ABSTRACT, ACC_STRICT, ACC_SYNTHETIC)
  
  override def flagExcludesFlags = List(
    (ACC_ABSTRACT, ACC_FINAL),
    (ACC_ABSTRACT, ACC_NATIVE),
    (ACC_ABSTRACT, ACC_PRIVATE),
    (ACC_ABSTRACT, ACC_STATIC),
    (ACC_ABSTRACT, ACC_STRICT),
    (ACC_ABSTRACT, ACC_SYNCHRONIZED)
  )
}

case class InterfaceMethodFlags(flags: Int) extends Flags with MethodFlagNames {
  def containerType = "interface method"
  def requiredFlags = List(ACC_ABSTRACT, ACC_PUBLIC)
  def optionalFlags = List(ACC_VARARGS, ACC_BRIDGE, ACC_SYNTHETIC)
}

case class ConstructorFlags(flags: Int) extends Flags with MethodFlagNames {
  def containerType = "constructor"
  def requiredFlags = Nil
  def optionalFlags = accessFlags ::: List(ACC_VARARGS, ACC_STRICT, ACC_SYNTHETIC)
}

