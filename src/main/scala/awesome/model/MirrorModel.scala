package awesome
package model

import javax.lang.model.element._
import javax.lang.model.util.ElementScanner6
import javax.lang.model.element.TypeElement
import javax.annotation.processing.{ ProcessingEnvironment, AbstractProcessor, RoundEnvironment }
import scala.collection.JavaConversions
import scala.collection.{ immutable, mutable }

class MirrorModel

// Element:     ExecutableElement, PackageElement, TypeElement, TypeParameterElement, VariableElement
//
// TypeMirror -> 
//   ReferenceType -> ArrayType, DeclaredType, ErrorType, NullType, TypeVariable
//
// Type:
//
// ArrayType  Represents an array type.
// DeclaredType   Represents a declared type, either a class type or an interface type.
// ErrorType  Represents a class or interface type that cannot be properly modeled.
// ExecutableType   Represents the type of an executable.
// NoType   A pseudo-type used where no actual type is appropriate.
// NullType   Represents the null type.
// PrimitiveType  Represents a primitive type.
// ReferenceType  Represents a reference type.
// TypeMirror   Represents a type in the Java programming language.
// TypeVariable   Represents a type variable.
// TypeVisitor<R,P>   A visitor of types, in the style of the visitor design pattern.
// WildcardType   Represents a wildcard type argument.
//
// TypeKinds: BOOLEAN BYTE SHORT INT LONG CHAR FLOAT DOUBLE VOID NONE NULL
//   ARRAY DECLARED ERROR TYPEVAR WILDCARD PACKAGE EXECUTABLE OTHER
//
// ElementKinds: PACKAGE ENUM CLASS ANNOTATION_TYPE INTERFACE ENUM_CONSTANT
//   FIELD PARAMETER LOCAL_VARIABLE EXCEPTION_PARAMETER METHOD CONSTRUCTOR
//   STATIC_INIT INSTANCE_INIT TYPE_PARAMETER OTHER
//
// util.Elements:
//
// element.Name util.Elements.getName(java.lang.CharSequence)
// boolean util.Elements.isDeprecated(element.Element)
// java.lang.String util.Elements.getDocComment(element.Element)
// element.PackageElement util.Elements.getPackageElement(java.lang.CharSequence)
// element.TypeElement util.Elements.getTypeElement(java.lang.CharSequence)
// java.util.Map util.Elements.getElementValuesWithDefaults(element.AnnotationMirror)
// element.Name util.Elements.getBinaryName(element.TypeElement)
// element.PackageElement util.Elements.getPackageOf(element.Element)
// java.util.List util.Elements.getAllMembers(element.TypeElement)
// java.util.List util.Elements.getAllAnnotationMirrors(element.Element)
// boolean util.Elements.hides(element.Element,element.Element)
// boolean util.Elements.overrides(element.ExecutableElement,element.ExecutableElement,element.TypeElement)
// java.lang.String util.Elements.getConstantExpression(java.lang.Object)
// void util.Elements.printElements(java.io.Writer,element.Element[])
// 
// util.Types:
//
// boolean util.Types.contains(type.TypeMirror,type.TypeMirror)
// type.TypeMirror util.Types.erasure(type.TypeMirror)
// boolean util.Types.isSameType(type.TypeMirror,type.TypeMirror)
// element.TypeElement util.Types.boxedClass(type.PrimitiveType)
// type.TypeMirror util.Types.capture(type.TypeMirror)
// element.Element util.Types.asElement(type.TypeMirror)
// boolean util.Types.isSubtype(type.TypeMirror,type.TypeMirror)
// boolean util.Types.isAssignable(type.TypeMirror,type.TypeMirror)
// boolean util.Types.isSubsignature(type.ExecutableType,type.ExecutableType)
// java.util.List util.Types.directSupertypes(type.TypeMirror)
// type.PrimitiveType util.Types.unboxedType(type.TypeMirror)
// type.PrimitiveType util.Types.getPrimitiveType(type.TypeKind)
// type.NullType util.Types.getNullType()
// type.NoType util.Types.getNoType(type.TypeKind)
// type.ArrayType util.Types.getArrayType(type.TypeMirror)
// type.WildcardType util.Types.getWildcardType(type.TypeMirror,type.TypeMirror)
// type.DeclaredType util.Types.getDeclaredType(element.TypeElement,type.TypeMirror[])
// type.DeclaredType util.Types.getDeclaredType(type.DeclaredType,element.TypeElement,type.TypeMirror[])
// type.TypeMirror util.Types.asMemberOf(type.DeclaredType,element.Element)
//
// ElementKind:
// ANNOTATION_TYPE
//           An annotation type.
// CLASS
//           A class not described by a more specific kind (like ENUM).
// CONSTRUCTOR
//           A constructor.
// ENUM
//           An enum type.
// ENUM_CONSTANT
//           An enum constant.
// EXCEPTION_PARAMETER
//           A parameter of an exception handler.
// FIELD
//           A field not described by a more specific kind (like ENUM_CONSTANT).
// INSTANCE_INIT
//           An instance initializer.
// INTERFACE
//           An interface not described by a more specific kind (like ANNOTATION_TYPE).
// LOCAL_VARIABLE
//           A local variable.
// METHOD
//           A method.
// OTHER
//           An implementation-reserved element.
// PACKAGE
//           A package.
// PARAMETER
//           A parameter of a method or constructor.
// STATIC_INIT
//           A static initializer.
// TYPE_PARAMETER
//           A type parameter.

//
// http://java.sun.com/javase/6/docs/api/javax/lang/model/element/package-summary.html
//
// http://java.sun.com/javase/6/docs/jdk/api/apt/mirror/com/sun/mirror/type/package-tree.html
//
// http://java.sun.com/javase/6/docs/api/javax/lang/model/util/ElementScanner6.html
// 

