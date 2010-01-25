package awesome
package jvm

import attr.{ InnerClass, InnerClasses, Exceptions, ScalaSig }

object ClassFilePrettyPrinter {
  def printMethods(
    comment: String,
    cond: MethodInfo => Boolean,
    ms: List[MethodInfo]
  ): (String, List[MethodInfo]) = {
    
    val (yes, no) = ms partition cond
    if (yes.isEmpty) ("", ms)
    else {
      val strs = yes sortBy (_.name) map (_.toString)
      val toPrint = if (comment.isEmpty) strs else ("// " + comment) :: strs
      
      ((toPrint map indentln).mkString + "\n", no)
    }
  }
  
  // def getConstructorStr(cf: ClassFile) = {
  //   val cs = cf.thisClass.reflectConstructors
  //   if (cs.isEmpty) ""
  //   else ("// constructors" :: (cs map (_.toString))) map indentln mkString
  // }
  
  def getMethodStr(cf: ClassFile) = {
    val mconds = 
      List[(String, MethodInfo => Boolean)](
        "constructors"            -> (x => (x.isConstructor)),
        "private methods"         -> (x => (x.isPrivate)),
        "package private methods" -> (x => (x.isPackagePrivate)),
        "public methods"          -> (x => !(x.isDeprecated)),
        "deprecated methods"      -> (_ => true)
      )
    
    (mconds.foldLeft(("", cf.methods)) { case ((s, undone), (comment, cond)) =>
      val (newString, left) = printMethods(comment, cond, undone)

      (s + newString, left)
    })._1
  }
  
  def getFieldStr(cf: ClassFile) = {
    if (cf.fields.isEmpty) ""
    else ("// fields" :: (cf.fields map (_.toString))) map indentln mkString
  }
  
  def getClassesStr(cf: ClassFile) = {
    if (cf.classes.isEmpty) ""
    else {
      val lines = cf.classes map (_.toScala) flatMap (_ split "\n" toList)
      ("// inner classes" :: lines) map indentln mkString
    }
  }
  
  def getScalaStr(cf: ClassFile) = cf.scalaSig map (x => indented(x.toSummaryString)) getOrElse ""

  private def indentln(x: Any) = "  " + x.toString + "\n"
  def apply(cf: ClassFile): String = {
    """
    |%s {
    |%s
    |%s
    |%s
    |%s
    |}
    |""".stripMargin.trim.format(
      cf.thisClass,
      getClassesStr(cf),
      getFieldStr(cf),
      getMethodStr(cf),
      getScalaStr(cf)
    )
  }
}
