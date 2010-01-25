package awesome
package validation

import io.{ Jar, Jars }
import pickler.{ Show, ScalaSigParser }
import jvm.{ ClassFile, ClassFileParser }
import jvm.attr.InnerClass
import scala.tools.nsc.io.{ File, Path }
import java.lang.reflect
import reflect.{ Proxy, InvocationHandler }

trait Validatable {
  def validate = validationFailures.isEmpty  
  def validationFailures: List[String]  
}

object Validate {
  def inner(pat: String) = new Validate(Jars.scala, pat) innerClasses
}

class Validate(val jars: Jars, pattern: String) {
  val classes = (jars grep pattern).classNames.toList
  
  // inner -> outer: these should be unique
  val isContainedIn = new HashMap[String, String]
  
  // inner -> list of classes which confirmed the above
  val verifiedBy = new HashMap[String, List[String]]
  // 
  // case class InnerClassRelation(claimant: String, outer: String, inner: String) { }
  // val innerClassClaims = new HashSet[InnerClassRelation]()
  
  def innerClasses = {
    val inners = new HashMap[ClassFile, Set[ClassFile]]
    var claims = 0
    var verified = 0
    
    for (claimant <- classes) {
      try {
        val pcf = ClassFileParser(claimant).get.process
        pcf.classes foreach { innerClazz =>
          (isContainedIn get innerClazz.fullNameStr) match {
            case Some(outer) =>
              if (outer == innerClazz.outerNameStr) {
                verifiedBy(innerClazz.fullNameStr) ::= claimant
                verified += 1
              }
              else println("Mismatch: %s != %s".format(outer, innerClazz.outerNameStr))
            case _      =>
              isContainedIn(innerClazz.fullNameStr) = innerClazz.outerNameStr
              verifiedBy(innerClazz.fullNameStr) = List(claimant)
              claims += 1
          }
        }
      }
      catch { case _ => }
    }
    println("Evaluated %d claims, with %d verifications".format(claims, verified))
    
      // pcf.classes foreach { inner =>
      //   inner match {
      //     case InnerClass(_, Some(ic), Some(oc), _, _) =>
      //       innerClassClaims += InnerClassRelation(claimant, oc.toRawString, ic.toRawString)
      //     case _ =>
      //   }
      // }
        
      // val innerPCF = inner.toClassFile
      // if (inners contains pcf) inners(pcf) += innerPCF
      // else inners(pcf) = Set(innerPCF)
    // }

    // for (claimant <- innerClassClaims map (_.claimant)) {
    //   for (InnerClassRelation(`claimant`, outer1, inner1) <- innerClassClaims) {
    //     
    //     
    //     for (InnerClassRelation(claimant2, outer2, inner2) <- innerClassClaims ; if claimant != claimant2) {
    //       if (outer1 == outer2 && inner1 == inner2)
    //         println("%s and %s agree: %s contains %s".format(claimant, claimant2, outer1, inner1))
    //     }
    //   }
    // }
    // inners
    // classes map (x => ClassFileParser(x).process.classes)
  }
}