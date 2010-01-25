package awesome
package jvm

import types.Base

object Sig {
  val JLObjectSig = ClassTypeSignature(
    Package(List("java", "lang")),
    SimpleClassTypeSignature("Object", Nil),
    Nil
  )
  
  trait AnySignature // Method vs. All Others
  
  sealed trait TypeSignature extends AnySignature { }
  sealed trait TypeArgument { }
  sealed trait FieldSignature extends TypeSignature { }
  sealed trait ThrowsSignature { }
  
  case class BaseTypeSignature(x: Base.Type) extends TypeSignature {
    override def toString = x.toString
  }
  
  trait ParenArgs {
    type ParenArgType
    def parenArgs: List[ParenArgType]
    
    protected def parenArgStr =
      parenArgs map (_.toString) mkString ("(", ", ", ")")
  }
  trait BracketArgs {
    type BracketArgType
    def bracketArgs: List[BracketArgType]
    
    protected def bracketArgStr =
      if (bracketArgs.isEmpty) ""
      else bracketArgs map (_.toString) mkString("[", ", ", "]")
  }
  
  /** TypeArguments */
  case class NamedTypeArgument(variance: Option[Char], arg: FieldSignature) extends TypeArgument {
    def varianceStr = variance match {
      case Some('+')  => "_ <: "
      case Some('-')  => "_ >: "
      case _          => ""
    }
    
    override def toString = varianceStr + arg
  }
  case object UnboundedWildcard extends TypeArgument {
    override def toString = "_"
  }
  
  /** FieldSignatures */
  case class ArrayTypeSignature(elem: TypeSignature) extends FieldSignature { 
    override def toString = "Array[" + elem + "]"
  }
  case class TypeVariableSignature(id: Ident) extends FieldSignature with ThrowsSignature {
    override def toString = id.toScalaString
  }

  // A class type signature gives complete type information for a class or interface type.
  // The class type signature must be formulated such that it can be reliably mapped to the
  // binary name of the class it denotes by erasing any type arguments and converting each ‘.’
  // character in the signature to a ‘$’ character.
  case class ClassTypeSignature(
    pkg: Package,
    name: SimpleClassTypeSignature,
    typeArgs: List[SimpleClassTypeSignature]
  ) extends FieldSignature with ThrowsSignature with BracketArgs {
    
    type BracketArgType = SimpleClassTypeSignature
    def bracketArgs = typeArgs
    
    def fqname = "" + (
      if (pkg.isEmpty) name
      else pkg + "." + name
    )
    override def toString = Ident(fqname).toScalaString
  }
  
  case class MethodTypeSignature(
    tparams: List[FormalTypeParameter],
    params: List[TypeSignature],
    returnType: TypeSignature,
    throws: List[ThrowsSignature]
  ) extends AnySignature with BracketArgs with ParenArgs {
    
    type BracketArgType = FormalTypeParameter
    type ParenArgType = TypeSignature
    
    def bracketArgs = tparams
    def parenArgs = params
    
    private def throwsStr = throws map ("@throws(classOf[%s])\n" format _) mkString
    
    override def toString =
      throwsStr + "def %%s%s%s: %s".format(
        bracketArgStr,
        parenArgStr,
        returnType
      )
  }
  
  case class FormalTypeParameter(id: Ident, bounds: List[FieldSignature]) {
    def boundsStr =
      if (bounds.isEmpty) ""
      else " <: " + bounds.mkString(", ")
    
    override def toString = "%s%s".format(id, boundsStr)
  }
  
  case class SimpleClassTypeSignature(id: Ident, typeArgs: List[TypeArgument]) extends BracketArgs {
    type BracketArgType = TypeArgument
    def bracketArgs = typeArgs
    
    override def toString = id.toScalaString + bracketArgStr
  }
  
  // A class signature, defined by the production ClassSignature, is used to encode type information
  // about a class declaration. It describes any formal type parameters the class might have, and lists
  // its (possibly parameterized) direct superclass and direct superinterfaces, if any.
  case class ClassSignature(
    tparams: List[FormalTypeParameter],    
    superClass: ClassTypeSignature,
    interfaces: List[ClassTypeSignature]
  ) extends AnySignature with BracketArgs {
    
    type BracketArgType = FormalTypeParameter
    def bracketArgs = tparams
    
    private def indent(s: String) = (" " * 8) + s + "\n"
    private def extendsStr    = if (superClass == JLObjectSig) "" else "\n" + indent("extends " + superClass)
    private def interfaceStr  = if (interfaces.isEmpty) "" else interfaces map (x => indent("with " + x)) mkString
        
    def name = "%s"
    override def toString = "class %s%s%s%s".format(name, bracketArgStr, extendsStr, interfaceStr)
  }
  
  case class Package(segments: List[Ident]) {
    override def toString = segments mkString "."
    def isEmpty = segments.isEmpty
  }
}
