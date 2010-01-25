package awesome

import scala.tools.nsc.symtab.Flags

package object pickler {
  def flagsToString(flags: Long) = Flags.flagsToString(flags)
  def pickledToRawFlags(flags: Long) = Flags.pickledToRawFlags(flags)
}
