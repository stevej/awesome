package awesome

package object jvm {
  final val JAVA_MAGIC = 0xCAFEBABE
  final val JAVA_MAJOR_VERSION = 45
  final val JAVA_MINOR_VERSION = 3
  
  // Constant pool tags
  final val CONSTANT_Utf8 = 1
  final val CONSTANT_Unicode = 2  // unimplemented
  final val CONSTANT_Integer = 3
  final val CONSTANT_Float = 4
  final val CONSTANT_Long = 5
  final val CONSTANT_Double = 6
  final val CONSTANT_Class = 7
  final val CONSTANT_String = 8
  final val CONSTANT_Fieldref = 9
  final val CONSTANT_Methodref = 10
  final val CONSTANT_InterfaceMethodref = 11
  final val CONSTANT_NameAndType = 12

  final val constantTagName = Map(
    CONSTANT_Utf8 -> "UTF8",
    CONSTANT_Unicode -> "Unicode",
    CONSTANT_Integer -> "Int",
    CONSTANT_Float -> "Float",
    CONSTANT_Long -> "Long",
    CONSTANT_Double -> "Double",
    CONSTANT_Class -> "class",
    CONSTANT_String -> "Asciz",
    CONSTANT_Fieldref -> "Field",
    CONSTANT_Methodref -> "Method",
    CONSTANT_InterfaceMethodref -> "InterfaceMethod",
    CONSTANT_NameAndType -> "NameAndType"
  )
  
  import java.lang.reflect.{ Array => _, _ }
  
  // private val jvmNamer = new parameter.BytecodeReadingParanamer
  // def parameterNamesFor(m: Method): List[String] =
  //   jvmNamer.parameterNames(m, false)

  def javaNameToScala(s: String) = {
    val javaNames = List("boolean", "byte", "short", "int", "long", "char", "float", "double")
    if (javaNames contains s) s.capitalize
    else if (s == "void") "Unit"
    else s
  }
  
  def boundsStr(bounds: Array[Type]): String = {
    val bs = bounds.toList map javaTypeToString filterNot (_ == "AnyRef")
    bs.mkString(" with ")
  }
  
  def wildString(x: WildcardType) = {
    val lo = boundsStr(x.getLowerBounds)
    val hi = boundsStr(x.getUpperBounds)
    
    val loStr = if (lo.isEmpty) "" else " >: " + lo
    val hiStr = if (hi.isEmpty) "" else " <: " + hi
    
    "_" + loStr + hiStr
  } 
  
  def tvarString(clazz: JClass[_]) = {
    def boundsStr(bounds: Array[Type]): String = {
      val bs = bounds.toList map javaTypeToString filterNot (_ == "AnyRef")
      
      if (bs.isEmpty) ""
      else bs.mkString(" <: ", " with ", "")
    }

    clazz.getTypeParameters.toList map (x => x.getName + boundsStr(x.getBounds filterNot (_ == clazz)))
  }
   
  private def parameterizedString(x: ParameterizedType) = {
    val namePart = x.getRawType match {
      case x: JClass[_]   => x.getName
      case _              => x.toString
    }
    val argPart = x.getActualTypeArguments.toList map javaTypeToString
    Ident.asScala(namePart, argPart)
  }
    
  private def arrayString(componentType: Type) = "Array[" + javaTypeToString(componentType) + "]"
  
  def javaTypeToString(t: Type): String = t match {
    case x: ParameterizedType   => parameterizedString(x)
    case x: GenericArrayType    => arrayString(x.getGenericComponentType())
    case x: WildcardType        => wildString(x)
    case x: TypeVariable[_]     => x.getName()      
    case x: JClass[_]           => 
      if (x.isPrimitive) javaNameToScala(x.toString)
      else if (x.isArray) arrayString(x.getComponentType)
      else Ident.asScala(x.getName, tvarString(x))

    case _                      => if (t == null) "" else t.toString
  }
}
