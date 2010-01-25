import awesome._
import Main._
import scala.tools.nsc.io._
import io.Jars
import scala.tools.nsc.interpreter._

// implicit def mkSigComplete(sig: pickler.ScalaSig): CompletionAware = {
//   new CompletionAware {
//     def completions = sig.context.entries map (_.tagName)
//   }
// }