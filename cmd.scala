import scala.tools.nsc.Global
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter._
import awesome._

import Main._
import io.Jars
import pickler.{ Pool, ScalaSig }

:power

// val t = repl.mkTree("def f(x: Int, y: Int) = f(5, 10) + f(10, 20)")
// def cc(x: Product): ProductCompletion = new ProductCompletion(x)
// val x = cc(t)
// 
// def xmlc(url: String) = XMLCompletion(xml.XML.load(new java.net.URL(url)))
// lazy val airport = XMLCompletion(xml.XML loadString Process("airport -xs").stdout.mkString)
// 
// lazy val tracAll = xmlc(
//   "https://lampsvn.epfl.ch/trac/scala/query?status=assigned&status=new&status=reopened&group=owner&format=rss" +
//   "&component=!Eclipse+plugin&order=priority&col=id&col=summary&col=component&col=type&col=priority"
// )
// 
// val connections = new CompletionAware {  
//   lazy val completions = Process("netstat -p tcp").toList drop 2 map (_ split "\\s+" apply 4)
//   override def execute(s: String) = Some(new scala.util.Random)
// }
