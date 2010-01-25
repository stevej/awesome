package awesome
package jvm

import parser._
import scala.util.parsing.combinator._
import types._

object DescriptorParser extends RegexParsers with ImplicitConversions with ParserUtil {
  lazy val binaryName = rep(elemExcept(';'))      ^^ (_.mkString)
  lazy val baseType   = elemOf("BCDFIJSZ": _*)    ^^ Base.apply 
  lazy val voidType   = 'V'                       ^^^ Base.Unit
  lazy val arrayType  = rep1('[') ~ fieldType     ^^ { case a~b => ArrayType(a.size, b) }
  lazy val objectType = 'L' ~> binaryName <~ ';'  ^^ Ident ^^ ObjectType
  
  lazy val methodType = ( '(' ~> rep(fieldType) <~ ')' ) ~ returnType ^^ MethodType
  lazy val fieldType: Parser[Type] = baseType | objectType | arrayType  
  lazy val returnType = fieldType | voidType
  
  def method(text: String): ParseResult[MethodType] = parseAll(methodType, text)
  def field(text: String): ParseResult[Type]        = parseAll(fieldType, text)
  def apply(text: String): ParseResult[Type]        = parseAll(fieldType | methodType, text)
}