package awesome
package model

import java.lang.reflect

object Java extends Model[reflect.Type] {
  type JavaTVar = reflect.TypeVariable[_ <: reflect.GenericDeclaration]
  
  def toType(x: reflect.Type): Type = new JavaType(x)
  def toMethod(x: reflect.Method): Method = new JavaMethod(x)
  def castToTypeVar(x: reflect.TypeVariable[_]) = toTypeVar(x.asInstanceOf[JavaTVar])
  def toTypeVar[T <: reflect.GenericDeclaration](x: reflect.TypeVariable[T]) = new JavaTypeVar(x)
  
  // class JavaMethodCall() extends MethodCall {
  // }
  
  trait HasTypeParams extends super.HasTypeParams {
    def underlying: reflect.GenericDeclaration
    def typeParams: List[TypeVar] = underlying.getTypeParameters.toList map castToTypeVar
  }
  
  class JavaTypeBounds(val lo: List[Type], val hi: List[Type]) extends TypeBounds {
  }
  
  class JavaTypeVar[T <: reflect.GenericDeclaration](val underlying: reflect.TypeVariable[T]) extends TypeVar {
    def name = underlying.getName
    def bounds: TypeBounds = new JavaTypeBounds(Nil, underlying.getBounds().toList map toType)
  }
  
  class JavaMethod(val underlying: reflect.Method) extends Method with HasTypeParams {
    def name = underlying.getName
    def formalParams: List[Type] = underlying.getGenericParameterTypes.toList map toType
    def returnType: Type = toType(underlying.getGenericReturnType)
  }
  
  class JavaField(val underlying: reflect.Field) extends Field {
    def name = underlying.getName
    def fieldType = toType(underlying.getGenericType)
  }
  
  class JavaType(val underlying: reflect.Type) extends super.Type {
    def name = underlying match {
      case x: JClass[_] => x.getName
    }
    def typeArgs: List[Type] = underlying match {
      case x: reflect.ParameterizedType => x.getActualTypeArguments.toList map toType
      case _                            => Nil
    }
    def <:<(other: Type): Boolean = (underlying, other.underlying) match {
      case (lhs: JClass[_], rhs: JClass[_]) => lhs isAssignableFrom rhs
      case _                                => false
    }
  }
}