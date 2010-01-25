package awesome
package pkg

protected[awesome] trait Utility {
  // Applies a function to a value and then returns the value.
  def returning[T](x: T)(f: T => Unit): T = { f(x) ; x }
  
  // Applies a function to a value and then returns the result.
  def applying[T, U](x: T)(f: T => U): U = f(x)
  
  // Applies a function to a value if condition is true; otherwise returns the value.
  def maybeApply[T](x: T, cond: Boolean)(f: T => T): T = if (cond) f(x) else x
  
  // If first argument is null, return second; otherwise first.
  def ifNull[T >: Null <: AnyRef](x: T, alt: => T): T = if (x eq null) alt else x
  
  // Reflection
  def forName(name: String): JClass[_] =
    try java.lang.Class.forName(name)
    catch { case _ => null }
  def forNameOpt(name: String): Option[JClass[_]] = Option(forName(name))
  
  // Tracing
  def tracing[T](msg: Any)(body: => T): T = {
    val res = body
    println("[%s] %s".format(msg, res))
    res
  }

  def indented(s: String, spaces: Int = 2) = s.replaceAll("(?m)^", " " * spaces)
  
  // If argument is "" or null, None.
  def nonEmptyString(x: String): Option[String] =
    if (x == null || x == "") None
    else Some(x)
  
  // Filter out empty Strings and then join with spaces
  def spaceSepString(xs: String*) = xs filterNot (_ == "") mkString " "
  
  // If empty "", otherwise "[t1, t2, ...]"
  // private def toScalaString(s: Any): String = Ident(s.toString).toScalaString
  
  def tparamsToString(xs: Seq[Any]) = 
    if (xs.isEmpty) "" else 
    xs.mkString("[", ", ", "]")
    
  // If empty "()", otherwise "(p1, p2, ...)"
  def paramsToString(xs: Seq[Any]) =
    xs.mkString("(", ", ", ")")
  
  def arrayToList[T](xs: Array[T]) = if (xs == null) Nil else xs.toList  
    
  def findMap[T, U](xs: Traversable[T])(pf: T =>? U): Option[U] = {
    for (x <- xs ; if pf isDefinedAt x)
      return Some(pf(x))
    
    None
  }
  def flattenIterator[A, B](it: Iterator[A])(implicit ev: A <:< Option[B]): Iterator[B] = new Iterator[B] {
    private var rest = it
    private def ff = rest = rest dropWhile (_.isEmpty)
    def hasNext = { ff ; rest.hasNext }
    def next = { ff ; rest.next.get }
  }
  
  def uniqueList[T](xss: Traversable[T]*): List[T] = (xss.foldLeft)(List[T]())(_ ++ _).removeDuplicates
  
  //
  def allprops = {
    import scala.collection.JavaConversions._
    
    val keys = (System.getProperties().propertyNames(): Iterator[_]).asInstanceOf[Iterator[String]]
    Map(keys.toList map (k => (k, System.getProperty(k))) : _*)
  }
}
