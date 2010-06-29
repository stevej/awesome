package awesome
package pickler

import scala.reflect.generic.PickleBuffer
import scala.tools.nsc.util.ShowPickled
import jvm.ClassFileParser

object Show {  
  def apply(name: String): Unit = apply(ScalaSig getBytes name)

  def apply(data: Array[Byte]): Unit = {
    val pickle = new PickleBuffer(data, 0, data.length)
    ShowPickled.printFile(pickle, Console.out)
  }
}
