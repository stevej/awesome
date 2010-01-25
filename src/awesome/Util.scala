package awesome

import java.lang.reflect
import reflect.{ Proxy, InvocationHandler }

/** Misc dumping ground for bits in progress */
object Util {
  // implicit def br(xs: Seq[Byte]) = new parser.ByteReader(xs)
  // def parse(xs: Seq[Byte])(p: ScalaSigParser => ScalaSigParser#Parser[_]) = p(new ScalaSigParser())(br(xs))
  
  /** Usage: singleton[Set.type] */
  def singleton[T](implicit man: Manifest[T]) = {
    val name = man.erasure.getName()
    assert(name endsWith "$", "Not an object: " + name)
    val clazz = forName(name)
  
    clazz.getField("MODULE$").get(clazz).asInstanceOf[T]
  }
  
  /** proxy experiments */
  def walkInterfaces(xs: List[JClass[_]]): List[JClass[_]] = {
    val xs2 = xs flatMap (_.getInterfaces)
    if ((xs2.toSet -- xs.toSet).nonEmpty) walkInterfaces((xs ++ xs2).removeDuplicates)
    else xs
  }
  
  def proxy[T <: AnyRef](x: T) = {
    val clazz = x.getClass
    val interfaces = walkInterfaces(clazz.getInterfaces.toList)

    val handler = new reflect.InvocationHandler {
      def invoke(proxy: Object, method: reflect.Method, args: Array[Object]): Object = {
        method.invoke(x, args: _*)
      }
    }
    reflect.Proxy.newProxyInstance(clazz.getClassLoader, Array[Class[_]](interfaces: _*), handler)
  }
  def unproxy(x: AnyRef) = reflect.Proxy.getInvocationHandler(x)
  
  /** More useful stack traces */
  case class StackElement(className: String, fileName: String, lineNumber: Int, methodName: String) { }
  
  def stackElements: List[StackElement] = {
    val elems = new Exception().getStackTrace.toList
    elems map (x => StackElement(x.getClassName, x.getFileName, x.getLineNumber, x.getMethodName))
  }
  def stackFilenames = stackElements map (_.fileName) removeDuplicates
  def stackClassnames = stackElements map (_.className) removeDuplicates
  def stackClazzes = stackClassnames flatMap forNameOpt
  def stackClazzes2 = Stream from 0 map sun.reflect.Reflection.getCallerClass takeWhile (_ != null) toList
}