package awesome
package jvm
package attr
  
case class ExceptionTable(
  startPC: Int, 
  endPC: Int, 
  handlerPC: Int, 
  catchType: Option[Ident]
)

// If the method is either native or abstract, its method_info
// structure must not have a Code attribute. Otherwise, its method_info
// structure must have exactly one Code attribute. 
case class Code(
  maxStack: Int,
  maxLocals: Int,
  code: Seq[Byte],
  exceptionTable: List[ExceptionTable],
  attributes: List[Attribute]
) extends AttributeClass("Code") with Attributed { 
  
  def lineNumberTable         = findAttr({ case x: LineNumberTable => x })
  def localVariableTable      = findAttr({ case x: LocalVariableTable => x })
  def localVariableTypeTable  = findAttr({ case x: LocalVariableTypeTable => x })
  def stackMapTable           = findAttr({ case x: StackMapTable => x })
  
  override def toString =
    "Code:\n   Stack=%d, Locals=%d, %d bytes\n".format(maxStack, maxLocals, code.size)
}