package awesome
package parser

import scala.util.parsing.combinator._

trait JVMParsers extends StandardBinaryParsers with StandardByteInterpretation with ImplicitConversions {  
  /** Reads a u2 and then repeats the given parser that many times */
  def collect[T](p: Parser[T]): Parser[List[T]] = u2 >> (x => repN(x, p))
  def collect_u1[T](p: Parser[T]): Parser[List[T]] = u1 >> (x => repN(x, p))
}