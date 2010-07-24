package awesome
package pkg

protected[awesome] trait ASM {
  import org.objectweb.asm.Type
  
  def asmTypesToString(xs: Seq[Type]): String =
    xs map asmTypeToString mkString ", "

  def asmTypeToString(x: Type): String = {
    import Type._
    
    def arrayTypeToString(x: Type) = {
      val it = Iterator.iterate(asmTypeToString(x.getElementType()))("Array[" + _ + "]")
      (it take x.getDimensions toList) last
    }
    
    val clazz = x.getSort() match {
      case VOID     => "Unit"
      case BOOLEAN  => "Boolean"
      case CHAR     => "Char"
      case BYTE     => "Byte"
      case SHORT    => "Short"
      case INT      => "Int"
      case FLOAT    => "Float"
      case LONG     => "Long"
      case DOUBLE   => "Double"
      case ARRAY    => arrayTypeToString(x)
      case _        => x.getInternalName.toExternal
    }
    
    Ident(clazz).toScalaString
  }
  
  def asmReturnType(desc: String) = Type.getReturnType(desc)
  def asmArgumentTypes(desc: String) = Type.getArgumentTypes(desc).toList
}