package awesome
package asm

import org.objectweb.asm._

object RefGen {
  import java.lang.reflect.{ ParameterizedType, GenericArrayType, TypeVariable, Array => JArray }
  
  trait UserType {
    def returnedClass(): JClass[_]
  }
  
  def getClassFromType(t: JType): Option[JClass[_]] = t match {
    case x: JClass[_]         => Some(x)
    case x: ParameterizedType => getClassFromType(x.getRawType())
    case x: GenericArrayType  => getClassFromType(x.getGenericComponentType) map (JArray.newInstance(_, 0).getClass())
    case _                    => None
  }
  
  def getTypeArguments(baseClass: JClass[_], childClass: JClass[_]): List[JClass[_]] = {
    val resolvedTypes = new HashMap[JType, JType]()
    
    var jtype: JType = childClass
    while (getClassFromType(jtype) != Some(baseClass)) {
      jtype match {
        // there is no useful information for us in raw types, so just keep going.
        case x: JClass[_]         =>
          jtype = x.getGenericSuperclass()
        case x: ParameterizedType =>
          val rawType = x.getRawType.asInstanceOf[JClass[_]]
          val actualTypeArguments: List[JType] = x.getActualTypeArguments.toList
          val typeParameters: List[TypeVariable[_]] = rawType.getTypeParameters.toList
          (typeParameters, actualTypeArguments).zipped foreach ((x, y) => resolvedTypes(x) = y)
          
          println((typeParameters, actualTypeArguments))
          println("rawType = %s baseClass = %s".format(rawType, baseClass))
        
          if (rawType != baseClass)
            jtype = rawType.getGenericSuperclass
      }
    }
    
    // def walkOne(jtype: Type): Type =    
    //   if (getClassFromType(jtype) == Some(baseClass)) jtype
    //   else walkOne(jtype match {
    //     // there is no useful information for us in raw types, so just keep going.
    //     case x: JClass[_]         => x.getGenericSuperclass()
    //     case x: ParameterizedType =>
    //       val rawType = x.getRawType.asInstanceOf[JClass[_]]
    //       (x.getActualTypeArguments, rawType.getTypeParameters).zipped foreach ((x, y) => resolvedTypes(x) = y)
    //     
    //       if (rawType == baseClass) jtype
    //       else rawType.getGenericSuperclass
    //   })
    // 
    // // start walking up the inheritance hierarchy until we hit baseClass    
    // val jtype = walkOne(childClass)
    // 
    // finally, for each actual type argument provided to baseClass, determine (if possible)
    // the raw class for that type argument.
    val actualTypeArguments: List[JType] = jtype match {
      case x: JClass[_]         => x.getTypeParameters().toList
      case x: ParameterizedType => x.getActualTypeArguments().toList
    }
    def getOneArgument(t: JType): JClass[_] =
      if (resolvedTypes contains t) getOneArgument(resolvedTypes(t))
      else getClassFromType(t).get
      
    actualTypeArguments map getOneArgument
  }
  
  abstract class AbstractUserType[T] extends UserType {
    def returnedClass() = getTypeArguments(classOf[AbstractUserType[_]], getClass()).head
  }
  
  abstract class TypeAwareArrayList[T <: AnyRef] extends java.util.ArrayList[T] {
    def toArrayFix(): Array[T] = {
      val tpe = getTypeArguments(classOf[TypeAwareArrayList[T]], getClass()).head
      toArray[T](JArray.newInstance(tpe, size).asInstanceOf[Array[T]])
    }
  }
}
