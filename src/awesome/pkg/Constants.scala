package awesome
package pkg

protected[awesome] trait Constants {
  import org.objectweb.asm.Opcodes._
  import org.objectweb.asm.util.CheckSignatureAdapter
  import org.objectweb.asm.ClassWriter
  
  final val CLASS_SIGNATURE = CheckSignatureAdapter.CLASS_SIGNATURE
  final val METHOD_SIGNATURE = CheckSignatureAdapter.METHOD_SIGNATURE
  final val TYPE_SIGNATURE = CheckSignatureAdapter.TYPE_SIGNATURE
  final val COMPUTE_FRAMES = ClassWriter.COMPUTE_FRAMES
  
  val opcodeNameMap = Map(
    INVOKEVIRTUAL -> "VIRTUAL",
    INVOKESPECIAL -> "SPECIAL",
    INVOKESTATIC -> "STATIC",
    INVOKEINTERFACE -> "INTERFACE",
    INVOKEDYNAMIC -> "DYNAMIC"
  )
  
  final val JL_OBJECT = "java/lang/Object"
  final val CLASS_INIT = "<init>"
  final val STATIC_INIT = "<clinit>"
  
  final val DEFAULT_JVM_VERSION = V1_1  // 45.3 
}