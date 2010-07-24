package awesome
package jvm

import java.lang.reflect
import reflect.{ Method, Type }

object Util {
  def paramTypes(m: MethodInfo) = m.reflectArgumentTypes.get
  def isMatchingArg(arg: Type, param: Type) = (arg, param) match {
    case (a: JClass[_], p: JClass[_]) => p isAssignableFrom a
    case _                            => false
  }
    
  // def isMatchingArg(arg: JClass[_], param: JClass[_]) = param isAssignableFrom arg
  def isMoreSpecific(m: MethodInfo, others: Seq[MethodInfo]) = {
    val mps = paramTypes(m)
    
    others forall { m2 => 
      (paramTypes(m2).size == mps.size) && 
      ((paramTypes(m2), mps).zipped forall isMatchingArg)
    }
  }
  
  def generateSignature(owner: String, name: String, args: Seq[Any]): String = {
    lazy val caller = CF.one(owner).get
    lazy val argTypes = args map (x => CF.one(x.toString).get)
    lazy val argClazzes = argTypes map (_.thisClass.clazz)
    
    def isPossibleMatch(m: MethodInfo) = {
      def varArgType = m.varArgType.get
      def numParamCheck: Seq[_] => Boolean = (xs: Seq[_]) => {
        if (m.isVarArgs) xs.size >= (paramTypes(m).size - 1)
        else paramTypes(m).size == xs.size
      }
        
      (m.name == name) &&
      numParamCheck(args) &&
      ((argClazzes, paramTypes(m)).zipped forall isMatchingArg) &&
      (!m.isVarArgs || ((argClazzes drop (paramTypes(m).size - 1) forall (x => isMatchingArg(x, varArgType)))))
    }
  
    (caller.methods filter isPossibleMatch) match {
      case Nil      => error("'%s' has no method named '%s'".format(owner, name))
      case List(x)  => x.descriptor.text
      case xs       => 
        // one last try
        val mostSpecific = xs filter (x => isMoreSpecific(x, xs filterNot (_ eq x)))
        
        if (mostSpecific.size == 1) mostSpecific.head.descriptor.text
        else error("'%s' has multiple members '%s' and disambiguation failed among: %s".format(
          owner, name, xs.mkString("\n    ", "\n    ", "")
        ))
    }
  }
}