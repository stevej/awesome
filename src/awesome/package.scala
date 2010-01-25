import awesome.io.{ Jar, Jars }

package object awesome extends pkg.Constants
                          with pkg.Types 
                          with pkg.Utility
                          with pkg.ASM 
                          with pkg.Pimps {
  
  implicit val identOrdering: Ordering[Ident] = Ordering[String] on (_.name)
  implicit def string2ident(s: String): Ident = Ident(s)

  // can't overload in package object
  // def anySorter[T](f: T => String): Ordering[T] = Ordering[String] on f
  def anySorter[T](): Ordering[T] = Ordering[String] on (_.toString)
}