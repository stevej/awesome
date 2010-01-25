package awesome
package asm

import org.objectweb.asm._
import Opcodes._
import java.lang.reflect

class OpenMethod(owner: String)(implicit mw: MethodVisitor) {
  lazy val targetClazz: JClass[_] = owner.clazz
  
  def fieldType(name: String) =
    targetClazz.getFields find (_.getName == name) map (_.getType)
  
  def typeOfField(name: String): String =
    fieldType(name) map Type.getDescriptor getOrElse error("'%s' has no field named '%s'".format(owner, name))
  
  //
  def calculateSignature(owner: String, name: String, args: Seq[Any]): String = {
    lazy val callerClazz = owner.clazz
    def argClasses: Seq[JClass[_]] = args map (_.asInstanceOf[AnyRef].getClass)
    def isMatchingArg(arg: JClass[_], param: JClass[_]) = param isAssignableFrom arg
    def isPossibleMatch(m: reflect.Method): Boolean = {
      if (m.getName != name) return false
      
      val params: List[JClass[_]] = m.getParameterTypes.toList
      
      if (m.isVarArgs) {
        val reqParams = params.size - 1
        (args.size >= reqParams) && {
          val reqPairs = (argClasses take reqParams, params take reqParams).zipped
          val varArgPairs = {
            val xs = argClasses drop reqParams
            (xs, List.fill[JClass[_]](xs.size)(params.last.getComponentType)).zipped
          }
          (reqPairs forall isMatchingArg) && (varArgPairs forall isMatchingArg)
        }
      }
      else {
        (args.size == params.size) && 
        ((argClasses, params).zipped forall isMatchingArg)
      }
    }
    def isMoreSpecific(m: reflect.Method, others: Seq[reflect.Method]): Boolean = {
      val mps = m.getParameterTypes.toList
      
      others forall { m2 => 
        (m2.getParameterTypes.size == mps.size) && 
        ((m2.getParameterTypes.toList, mps).zipped forall (_ isAssignableFrom _))
      }
    }
    
    (callerClazz.getMethods.toList filter isPossibleMatch) match {
      case Nil      => error("'%s' has no method named '%s'".format(owner, name))
      case List(x)  => Type.getMethodDescriptor(x)
      case xs       => 
        // one last try
        val mostSpecific = xs filter (x => isMoreSpecific(x, xs filterNot (_ eq x)))
        
        if (mostSpecific.size == 1) Type.getMethodDescriptor(mostSpecific.head)
        else error("'%s' has multiple members '%s' and disambiguation failed among: %s".format(
          owner, name, xs.mkString("\n    ", "\n    ", "")
        ))
    }
  }

  class UnevaluatedMethod(opcode: Int, name: String, sig: String) {        
    def isVoid(s: String) = s endsWith "V"
    def signature(args: Seq[Any]) = ifNull(sig, calculateSignature(owner, name, args))

    def apply(args: Any*) = {
      args foreach (mw visitLdcInsn _)
      val s = signature(args)
      mw.visitMethodInsn(opcode, owner, name, s)
    }
  }
  
  def getStatic(name: String, desc: String = null): String = {
    val t = ifNull(desc, typeOfField(name))
    mw.visitFieldInsn(GETSTATIC, owner, name, t)
    
    (Type getType t getClassName).toInternal
  }
  def invokeStatic(name: String, sig: String = null) = new UnevaluatedMethod(INVOKESTATIC, name, sig)
  def invokeVirtual(name: String, sig: String = null) = new UnevaluatedMethod(INVOKEVIRTUAL, name, sig)
}
