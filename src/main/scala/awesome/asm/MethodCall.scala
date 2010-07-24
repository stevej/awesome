package awesome
package asm

import org.objectweb.asm._
import jvm.Descriptor

case class MethodCall(opcode: Int, owner: String, name: String, desc: Descriptor) { 
  def opcodeName = opcodeNameMap.getOrElse(opcode, "???")
  override def toString() = "MethodCall(%s, %s, %s, %s)".format(
    opcodeName, owner, name, desc
  )
}
