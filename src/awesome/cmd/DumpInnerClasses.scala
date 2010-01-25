package awesome
package cmd

import scala.tools.nsc.io.File
import io.{ Jar, Jars }
import jvm.ClassFileParser
import jvm.attr.{ InnerClasses, InnerClass }

object DumpInnerClasses {
  def main(args: Array[String]): Unit = {
    // default to scala library jars
    val jars =
      if (args.isEmpty) Jars.scala
      else Jars.default grep (x => args exists (_ contains x.name))
    
    val (iterator, fail) = innerClasses(jars)
      
    iterator foreach println
    fail() foreach println
  }

  abstract class Data
  case class Failure(msg: String) extends Data {
    override def toString = ">>> " + msg
  }
  case class OneInner(id: Ident, inner: InnerClass) extends Data {
    def p(x: Option[Ident]) = x map (_.name) getOrElse "_"
    override def toString = "%s %s %s %s".format(
      id.name, p(inner.innerClass), p(inner.outerClass), p(inner.innerName)
    )        
  }
    
  def innerClasses(jars: Jars) = {
    val failures = new ListBuffer[Data]
    def jarnames = jars.classNames.toList sortWith (_ < _)

    def doParse(name: String) = {
      val res = ClassFileParser opt name
      if (res.isEmpty)
        failures += Failure(name + " failed: " + res.toString)
        
      res
    }

    val pairs = 
      for {
        name <- jarnames.iterator
        val id = Ident(name)
        rcf <- doParse(name).iterator
        val parser = rcf.attributeParser
        attr <- rcf.attributes.iterator map (x => parser(x, id))
      }
      yield attr match {
        case InnerClasses(xs) => xs.iterator map (x => (id, x))
        case _                => Iterator.empty
      }

    // failures.toList ::: (pairs.flatten map OneInner.tuple).toList
    (pairs.flatten map OneInner.tupled, () => failures.toList)
  }
}


object ReadInnerClasses {
  // scala/Array scala/Array$$anon$2 scala/Array $anon$2
  // scala/Array scala/Predef$DummyImplicit scala/Predef DummyImplicit
  // scala/Array scala/Array$$anonfun$fill$4 scala/Array $anonfun$fill$4
  // scala/Array scala/Array$$anonfun$fill$3 scala/Array $anonfun$fill$3
  // scala/Array scala/Array$$anonfun$fill$2 scala/Array $anonfun$fill$2
  
  case class Inner(inner: String, outer: String, fqname: String, says: String) {
    override def toString = "%s: %s=%s of %s".format(
      says, inner, fqname, outer
    )    
  }
  
  def doUpdate[K, V](map: HashMap[K, List[V]], key: K, value: V) = {
    if (map contains key) map(key) ::= value
    else map(key) = List(value)
  }
  
  def corresponds(outer: String, containing: List[String]): List[String] = {
    // val xs = 
    (for (inner <- containing) yield {
      containedBy.get(inner) match {
        case Some(containers) =>
          val names = (containers map (_.outer)).removeDuplicates
          assert(names.size == 1)
          val name = names.head
          
          if (outer == name) List[String]("%s and %s correspond".format(outer, inner))
          else List[String](">>> %s -> %s but %s <- %s".format(outer, inner, inner, name))
        case None             =>
          List[String](">>> %s contains %s but it does not appear in containedBy".format(outer, inner))
      }
    }).flatten
    
    // xs.flatten
  }
  
  val containedBy = new HashMap[String, List[Inner]]
  val containsThese = new HashMap[String, List[Inner]]
  
  def checkContainedBy = for ((k, values) <- containedBy) yield {
    val outers = values map (_.outer)
    val unique = outers.removeDuplicates

    if (unique.size == 1)
      "%s is contained by %s (%d classes agree)".format(k, unique.head, outers.size)
    else (
      ">>> %s is contained by\n" + (
        for ((k1, v1) <- values.groupBy(_.outer)) yield
          ">>>   %s according to: %s".format(k1, v1 mkString ", ")
      ).mkString
    )
  }

  def checkContainsThese: List[String] = containsThese.toList map { 
    case (k, values) =>
      val inners = values map (_.fqname)
      val xs = inners.removeDuplicates sortWith (_ < _)
  
      corresponds(k, xs)
  } flatten

  def main(args: Array[String]): Unit = {
    val lines = File(args(0)).lines()
    
    for (line <- lines) {
      val Array(clazz, full, outer, inner) = line split ' '
      val x = Inner(inner, outer, full, clazz)
      
      doUpdate(containedBy, full, x)
      doUpdate(containsThese, outer, x)
    }
    
    val res1 = checkContainedBy.toList sortWith (_ < _)
    val res2 = checkContainsThese.toList sortWith (_ < _)
    val all = res1 ::: res2
    
    val errors = all filter (_ startsWith ">>>")
    
    if (errors.isEmpty) println("All %d files consistent.".format(all.size))
    else errors foreach println
  }
}

