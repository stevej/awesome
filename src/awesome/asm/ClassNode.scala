package awesome
package asm

import org.objectweb.asm._
import Opcodes._

class ClassNode(cn: tree.ClassNode) {
  def access = cn.access
  def fields: List[tree.FieldNode] = cn.fields
  def methods: List[tree.MethodNode] = cn.methods
  def interfaces: List[String] = cn.interfaces
  def innerClasses: List[tree.InnerClassNode] = cn.innerClasses
}

object Analyze
{  
  def apply(jars: String*): List[ClassNode] = {
    val group = io.Jars.fromStrings(jars: _*)
    val readers = group.classNames map (x => new ClassReader(x))
    
    readers map (cr => new ClassNode(returning(new tree.ClassNode)(cn => cr.accept(cn, 0)))) toList
  }

  def main(args: Array[String]): Unit = {
  }
}
