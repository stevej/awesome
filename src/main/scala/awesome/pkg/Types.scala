package awesome
package pkg

protected[awesome] trait Types {
  // scala thou hast forsaken me
  type =>?[-A, +B] = PartialFunction[A, B]
  
  // Types which are annoying to import everywhere
  type IOException = java.io.IOException
  
  // Mutable collections with unambiguous mutablish names
  type ListBuffer[A] = scala.collection.mutable.ListBuffer[A]
  val ListBuffer = scala.collection.mutable.ListBuffer  
  type HashSet[A] = scala.collection.mutable.HashSet[A]
  val HashSet = scala.collection.mutable.HashSet
  type HashMap[A, B] = scala.collection.mutable.HashMap[A, B]
  val HashMap = scala.collection.mutable.HashMap
  
  // Some commonly reused names we want to globally disambiguate
  type ASMType = org.objectweb.asm.Type
  type ASMMethod = org.objectweb.asm.commons.Method
  
  type JClass[T] = java.lang.Class[T]
  type JType = java.lang.reflect.Type
  type JMethod = java.lang.reflect.Method
  
  // Types which nobody is going to confuse
  // val ScalaSigParser = pickler.ScalaSigParser
  // val ClassFileParser = jvm.ClassFileParser
  // type ByteReader = parser.ByteReader
}