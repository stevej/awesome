package awesome
package types

abstract class Type

case class ArrayType(dim: Int, elem: Type) extends Type {
  override def toString = (Iterator.iterate(elem.toString)("Array[" + _ + "]") take (dim + 1)).toList last
}
  
case class ObjectType(id: Ident) extends Type {
  def name = id.name
  override def toString = id.name.toString
}

case class MethodType(argumentTypes: List[Type], returnType: Type) extends Type {
  override def toString = "(%s)%s".format(argumentTypes mkString ", ", returnType)
}