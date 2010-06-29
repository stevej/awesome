import awesome.io.{ Jar, Jars }

package object awesome extends pkg.Constants
                          with pkg.Types 
                          with pkg.Utility
                          with pkg.ASM 
                          with pkg.Pimps  {
  
  implicit val identOrdering: Ordering[Ident] = Ordering[String] on (_.name)
  implicit def string2ident(s: String): Ident = Ident(s)
}