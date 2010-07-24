package awesome
package jvm

import java.lang.reflect

class JavaClass(val clazz: JClass[_]) {  
  lazy val methods = uniqueList(clazz.getMethods, clazz.getDeclaredMethods)
  lazy val constructors = uniqueList(clazz.getConstructors, clazz.getDeclaredConstructors) filterNot (_.isSynthetic)
  lazy val interfaces = clazz.getGenericInterfaces
  lazy val declaringClass = Option(clazz.getDeclaringClass)
  lazy val enclosingClass = Option(clazz.getEnclosingClass)
  lazy val enclosingMethod = Option(clazz.getEnclosingMethod)
  
  lazy val descMap = methods groupBy (Descriptor getMethodDescriptor _)
  def findMethodInfo(minfo: MethodInfo): Option[reflect.Method] =
    (descMap get minfo.descriptor.text) flatMap { methods =>
      methods find (_.getName == minfo.name.toEncoded)
    }
}