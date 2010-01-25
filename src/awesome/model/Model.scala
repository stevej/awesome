package awesome
package model

trait Model[T] {
  trait Named {
    def name: String
  }
  
  trait HasTypeParams {
    def typeParams: List[TypeVar]
  }

  trait Type extends Named {
    def underlying: T
    def typeArgs: List[Type]
    def <:<(other: Model.this.Type): Boolean
    // def intersect(other: Type): Option[Type]
  }

  trait TypeBounds {
    def lo: List[Type]
    def hi: List[Type]
  }

  trait TypeVar extends Named {
    def bounds: TypeBounds
  }

  trait Field extends Named {
    def fieldType: Type
  }

  trait Method extends Named with HasTypeParams {
    def typeParams: List[TypeVar]
    def formalParams: List[Type]
    def returnType: Type
    
    // def overrides(other: Method): Boolean
  }

  trait MethodCall {
    def typeArgs: List[Type]
    def args: List[Type]
  }
}
