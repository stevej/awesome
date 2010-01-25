package awesome
package cmd

import scala.tools.nsc.io._
import awesome.io._
import awesome.jvm.{ ClassFileParser, ClassFileCache }

object ShowRefs {
  def main(args: Array[String]): Unit = {
    val map = new HashMap[String, List[String]]
    
    def f(s: String) = {
      val (k, v) = s.lastIndexOf(".") match {
        case -1   => (s, "")
        case idx  => (s take idx, s drop (idx + 1))
      }
      
      if (map contains k) map(k) ::= v
      else map(k) = List(v)
    }
    
    for (arg <- args ; jar <- Jar(arg)) {
      val res = jar.referenced.dependencies
      res.toList foreach f
      
      for ((k, v) <- (map.toList sortWith (_._1 < _._1))) {
        if (v contains "") println(k)
        val xs = v filterNot (_ == "")
        if (xs.nonEmpty) {
          if (xs.size == 1) println(k + "." + xs.head)
          else println(k + ".{ " + xs.sortWith(_ < _).mkString(", ")  + " }")
        }
      }
      
      // println("%s references packages:\n  %s".format(arg, res.toList sortWith (_ < _) mkString ", "))
    }
  }
}
