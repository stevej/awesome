package awesome
package pkg

import io.{ Jar, Jars }

protected[awesome] trait Pimps {  
  import scala.util.parsing.combinator.Parsers
  
  implicit def option2Iterator[T](x: Option[T]): Iterator[T] =
    if (x.isDefined) Iterator.single(x.get) else Iterator.empty
  
  implicit def enrichList[A](x: List[A]): RichList[A] = new RichList(x)
  implicit def enrichIterator[A](x: Iterator[A]): RichIterator[A] = new RichIterator(x)
  implicit def enrichParseResult[T](x: Parsers#ParseResult[T]) = new RichParseResult[T](x)
  implicit def enrichFunction1[T, R](f: T => R): RichFunction1[T, R] = new RichFunction1(f)
  implicit def enrichBooleanFunction1[T](f: T => Boolean): RichBooleanFunction1[T] =
    new RichBooleanFunction1(f)
    
  /** Below this point are all probably temporary. */
  implicit def regexp2Jars(re: scala.util.matching.Regex): Jars =
    Jars.default grep re  
    
  implicit def jvmClassFileFunction12StringFunction1[T](f: jvm.ClassFile => T): String => T =
    (x: String) => f(string2jvmClassFile(x))
    
  implicit def stringList2jvmClassFileList(names: List[String]): List[jvm.ClassFile] =
    names map string2jvmClassFile
    
  implicit def string2jvmClassFile(name: String): jvm.ClassFile =
    jvm.ClassFileParser(name).get.process
    
  implicit def javaClazz2jvmClassFile(clazz: JClass[_]): jvm.ClassFile =
    jvm.ClassFileParser(clazz).get.process
  
  /** Enrichment classes. */
  
  class RichParseResult[T](p: Parsers#ParseResult[T]) {    
    def toOption: Option[T] = if (p.successful) Some(p.get) else None
    
    def getOrElseMsg(alt: String => T): T = 
      if (p.successful) p.get
      else alt(p.asInstanceOf[Parsers#NoSuccess].msg)
  }

  class RichFunction1[-T, +R](f: T => R) {
    def ifAndThen[R1 >: R](cond: R => Boolean, then: R => R1): T => R1 = (x: T) => {
      val res = f(x)
      if (cond(res)) then(res) else res
    }
  }
  
  class RichBooleanFunction1[-T](f: T => Boolean) {
    def &&[U <: T](g: U => Boolean) = (x: U) => f(x) && g(x)
    def ||[U <: T](g: U => Boolean) = (x: U) => f(x) || g(x)
  }
    
  class RichList[A](list: List[A]) {
    def uniq: List[A] = list.distinct

    def uniqmap[B](f: A => B): List[B] = (list map f).distinct
    def uniqfmap[B](f: A => Traversable[B]): List[B] = (list flatMap f).uniq
    
    def uniqsortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = uniq sorted (ord on f)
    def uniqsort(implicit ord: Ordering[A]): List[A] = uniq sorted ord
  }
  
  class RichIterator[+A](it: Iterator[A]) {
    def uniq: Iterator[A] = it.toList.distinct.iterator
    
    def uniqmap[B](f: A => B): Iterator[B] = (it map f).uniq
    def uniqfmap[B](f: A => Iterator[B]): Iterator[B] = (it flatMap f).uniq      
  }
}