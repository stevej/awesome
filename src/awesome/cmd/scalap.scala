package awesome
package cmd

import scala.tools.nsc.io.File
import io.{ Jar, Jars }
import jvm.ClassFileParser
import jvm.attr.{ InnerClasses, InnerClass }

// scalap scala.collection.MapLike 
// 
// CLASSPATH = merged classpath (directory classpath: .)
// FILENAME = ./scala/collection/MapLike.class
// package scala.collection
// trait MapLike[A >: scala.Nothing <: scala.Any, +B >: scala.Nothing <: scala.Any, +This >: scala.Nothing <: scala.collection.MapLike[A, B, This] with scala.collection.Map[A, B]] extends java.lang.Object with scala.PartialFunction[A, B] with scala.collection.IterableLike[scala.Tuple2[A, B], This] with scala.collection.generic.Subtractable[A, This] with scala.ScalaObject {
//   def $init$() : scala.Unit = { /* compiled code */ }
//   def empty : This
//   def get(key : A) : scala.Option[B]
//   def iterator : scala.collection.Iterator[scala.Tuple2[A, B]]
//   def +[B1 >: B <: scala.Any](kv : scala.Tuple2[A, B1]) : scala.collection.Map[A, B1]
//   def -(key : A) : This
//   override def isEmpty : scala.Boolean = { /* compiled code */ }
//   def getOrElse[B1 >: B <: scala.Any](key : A, default : => B1) : B1 = { /* compiled code */ }
//   def apply(key : A) : B = { /* compiled code */ }
//   def contains(key : A) : scala.Boolean = { /* compiled code */ }
//   def isDefinedAt(key : A) : scala.Boolean = { /* compiled code */ }
//   def keySet : scala.collection.Set[A] = { /* compiled code */ }
//   protected class DefaultKeySet extends java.lang.Object with scala.collection.Set[A] with scala.ScalaObject {
//     def this() = { /* compiled code */ }
//     def contains(key : A) : scala.Boolean = { /* compiled code */ }
//     def iterator : scala.collection.Iterator[A] = { /* compiled code */ }
//     def +(elem : A) : scala.collection.Set[A] = { /* compiled code */ }
//     def -(elem : A) : scala.collection.Set[A] = { /* compiled code */ }
//     override def size : scala.Int = { /* compiled code */ }
//     override def foreach[C >: scala.Nothing <: scala.Any](f : scala.Function1[A, C]) : scala.Unit = { /* compiled code */ }
//   }
//   def keysIterator : scala.collection.Iterator[A] = { /* compiled code */ }
//   def keys : scala.collection.Iterator[A] = { /* compiled code */ }
//   def valuesIterable : scala.collection.Iterable[B] = { /* compiled code */ }
//   protected class DefaultValuesIterable extends java.lang.Object with scala.collection.Iterable[B] with scala.ScalaObject {
//     def this() = { /* compiled code */ }
//     def iterator : scala.collection.Iterator[B] = { /* compiled code */ }
//     override def size : scala.Int = { /* compiled code */ }
//     override def foreach[C >: scala.Nothing <: scala.Any](f : scala.Function1[B, C]) : scala.Unit = { /* compiled code */ }
//   }
//   def valuesIterator : scala.collection.Iterator[B] = { /* compiled code */ }
//   def values : scala.collection.Iterator[B] = { /* compiled code */ }
//   def default(key : A) : B = { /* compiled code */ }
//   def filterKeys(p : scala.Function1[A, scala.Boolean]) : java.lang.Object with scala.collection.DefaultMap[A, B] = { /* compiled code */ }
//   def mapValues[C >: scala.Nothing <: scala.Any](f : scala.Function1[B, C]) : java.lang.Object with scala.collection.DefaultMap[A, C] = { /* compiled code */ }
//   def mapElements[C >: scala.Nothing <: scala.Any](f : scala.Function1[B, C]) : java.lang.Object with scala.collection.DefaultMap[A, C] = { /* compiled code */ }
//   def updated[B1 >: B <: scala.Any](key : A, value : B1) : scala.collection.Map[A, B1] = { /* compiled code */ }
//   def +[B1 >: B <: scala.Any](kv1 : scala.Tuple2[A, B1], kv2 : scala.Tuple2[A, B1], kvs : scala.Tuple2[A, B1]*) : scala.collection.Map[A, B1] = { /* compiled code */ }
//   def ++[B1 >: B <: scala.Any](kvs : scala.collection.Traversable[scala.Tuple2[A, B1]]) : scala.collection.Map[A, B1] = { /* compiled code */ }
//   def ++[B1 >: B <: scala.Any](iter : scala.collection.Iterator[scala.Tuple2[A, B1]]) : scala.collection.Map[A, B1] = { /* compiled code */ }
//   override def addString(b : scala.collection.mutable.StringBuilder, start : scala.Predef.String, sep : scala.Predef.String, end : scala.Predef.String) : scala.collection.mutable.StringBuilder = { /* compiled code */ }
//   override def stringPrefix : scala.Predef.String = { /* compiled code */ }
//   override def toString() : scala.Predef.String = { /* compiled code */ }
//   override def hashCode() : scala.Int = { /* compiled code */ }
//   override def equals(that : scala.Any) : scala.Boolean = { /* compiled code */ }
// }

import scala.tools.nsc.io._
import awesome.io.ByteCode
import awesome.jvm.ClassFileParser

object scalap {
  
  def usageMsg = """
    |usage: scalap {<option>} <name>
    |  -private           print private definitions
    |  -verbose           print out additional information
    |  -version           print out the version number of scalap
    |  -help              display this usage message
    |  -classpath <path>  specify where to find user class files
    |  -cp <path>         specify where to find user class files
  """.trim.stripMargin
    
  def usage = Console println usageMsg
  
  def main(args: Array[String]): Unit = {
    if (args.isEmpty)
      return usage
      
    val (paths, others) = args.toList partition (x => Path(x).exists)
    val targets: List[(String, List[Byte])] =
      (paths map (x => (x, File(x).bytes().toList))) :::
      (others map (x => (x, ByteCode(x).toList)))

    val classFiles = targets map (x => ClassFileParser(x._2).toOption)
    classFiles.flatten map (_.process) foreach println
  }
}
