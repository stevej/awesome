package awesome
package jvm

import parser._
import scala.util.parsing.combinator._
import types.Base

// Given declaration A<T>
//   T is a type variable 
//   T[] is a generic array type
//   A<String> is a parameterized type
object SignatureParser extends RegexParsers with ImplicitConversions with ParserUtil {  
  // Names of methods, fields and local variables are stored as unqualified names.
  // Unqualified names must not contain the characters '.', ';', '[' or '/'. Method
  // names are further constrained so that, with the exception of the special
  // method names (§3.9) <init> and <clinit>, they must not contain the characters
  // '<' or '>'.
  //
  // XXX what about ':'
  lazy val Identifier: Parser[Ident] = (
      "<init>"                        ^^^ Ident.initIdent
    | "<clinit>"                      ^^^ Ident.clinitIdent
    | rep1(elemExcept(":.;[/<>": _*))  ^^ (_.mkString) ^^ Ident
  )

  lazy val BaseType: Parser[Base.Type] = anyElem ^? Base.primitive

  lazy val ClassSignature: Parser[Sig.ClassSignature] = (
    FormalTypeParameters ~    // type parameters 
    ClassTypeSignature ~      // superclass
    rep(ClassTypeSignature)   // interfaces
  ) ^^ Sig.ClassSignature

  lazy val FormalTypeParameter  = Identifier ~ Bounds ^^ Sig.FormalTypeParameter
  lazy val FormalTypeParameters = '<' ~> rep(FormalTypeParameter) <~ '>' | success(Nil)
  
  lazy val Bounds         = ClassBound ~ rep(InterfaceBound) ^^ { case a~b => a.toList ::: b }
  lazy val ClassBound     = ':' ~> opt(FieldSignature)
  lazy val InterfaceBound = ':' ~> FieldSignature    
  
  lazy val FieldSignature: Parser[Sig.FieldSignature] = (
      ClassTypeSignature
    | ArrayTypeSignature
    | TypeVariableSignature
  )

  lazy val PackageSpecifier = rep(Identifier <~ '/') ^^ Sig.Package
  lazy val SimpleClassTypeSignature = Identifier ~ TypeArguments ^^ Sig.SimpleClassTypeSignature
  
  lazy val ClassTypeSignature: Parser[Sig.ClassTypeSignature] =
    'L' ~> PackageSpecifier ~ rep1sep(SimpleClassTypeSignature, '.') <~ ';' ^^
      { case a~b => Sig.ClassTypeSignature(a, b.head, b.tail) }

  lazy val TypeVariableSignature = 'T' ~> Identifier <~ ';' ^^ Sig.TypeVariableSignature
  lazy val TypeArguments = ( '<' ~> rep(TypeArgument) <~ '>' ) | success(Nil)

  lazy val TypeArgument: Parser[Sig.TypeArgument] = (
      opt(elemOf('+', '-')) ~ FieldSignature ^^ Sig.NamedTypeArgument
    | '*' ^^^ Sig.UnboundedWildcard
  )

  lazy val ArrayTypeSignature = '[' ~> TypeSignature ^^ Sig.ArrayTypeSignature
    
  lazy val TypeSignature: Parser[Sig.TypeSignature] = (
      FieldSignature
    | BaseType ^^ Sig.BaseTypeSignature
  )
  
  lazy val params = '(' ~> rep(TypeSignature) <~ ')'
  
  // foo: [T,U](x: T,y: U)java.lang.String
  // foo: (x: String,y: String)java.lang.String
  lazy val MethodTypeSignature: Parser[Sig.MethodTypeSignature] = (
    FormalTypeParameters ~ 
    params ~ 
    ReturnType ~ 
    rep(ThrowsSignature) 
  ) ^^ Sig.MethodTypeSignature

  lazy val VoidType = 'V' ^^^ Sig.BaseTypeSignature(Base.Unit)
  lazy val ReturnType: Parser[Sig.TypeSignature] = TypeSignature | VoidType
  lazy val ThrowsSignature: Parser[Sig.ThrowsSignature] = '^' ~> (ClassTypeSignature | TypeVariableSignature)
  
  lazy val root: Parser[Sig.AnySignature] = (
      ClassSignature
    | MethodTypeSignature
    | TypeSignature
  )
  
  def jclass(in: String) = parseAll(ClassSignature, in) getOrElseMsg error
  def field(in: String) = parseAll(FieldSignature, in) getOrElseMsg error
  def method(in: String) = parseAll(MethodTypeSignature, in) getOrElseMsg error
  
  def apply(sig: Signature): Sig.AnySignature = apply(sig.text)
  def apply(text: String): Sig.AnySignature = parseAll(root, text) getOrElseMsg error
}
