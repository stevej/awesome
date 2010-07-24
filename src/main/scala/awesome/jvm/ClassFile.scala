package awesome
package jvm

import attr.{ InnerClass, InnerClasses, Exceptions, ScalaSig }
import ClassFileParser.{ constant_pool, cp_class }

case class ClassFile private[jvm] (
  pool: ParsedPool,
  thisClass: ClassInfo,
  allFields: List[FieldInfo],
  allMethods: List[MethodInfo]
) extends ScalaSigAttributed {
  
  def className = name
  def allClasses = attributes flatMap { case InnerClasses(xs) => xs ; case _ => Nil }
  def allMembers: List[Member] = thisClass :: allFields ::: allMethods ::: allClasses
  
  def methods = allMethods filter methodFilter sorted
  def fields = allFields filter fieldFilter sortBy (_.toString)
  def classes = allClasses filter (_.innerClass.isDefined) sortBy (_.toString)
  def members = methods ::: fields ::: classes
  
  def referencedClasses = pool.referencedClasses
  def referencedPackages = pool.referencedPackages

  def isTopLevel = !(name contains "$")
  def mainMethod: Option[MethodInfo] = methods find (_.isJavaMain)
  
  def attributes        = thisClass.attributes
  def methodAttributes  = methods flatMap (_.attributes)
  def codeAttributes    = methods flatMap (_.code) flatMap (_.attributes)
  def fieldAttributes   = fields flatMap (_.attributes)  
  def allAttributes     = attributes ::: methodAttributes ::: codeAttributes ::: fieldAttributes
   
  def attribute(name: String) = thisClass.attributes find (_.name == name)
  def name = thisClass.name
  def pkg = thisClass.pkg
  def companion = thisClass.companion

  def isScalaObject = thisClass.isScalaObject
  def enclosingMethod = thisClass.enclosingMethod

  private def methodFilter: MethodInfo => Boolean = _.shouldPrint
  private implicit def methodSorter: Ordering[MethodInfo] = Ordering fromLessThan { (x: MethodInfo, y: MethodInfo) =>
    (x.isConstructor && !y.isConstructor) ||  
    (x.isPrivate && !y.isPrivate) ||
    (x.isPackagePrivate && !y.isPackagePrivate) ||
    (!x.isDeprecated  && y.isDeprecated) ||
    (x.name.toString < y.name.toString)
  }
  private def fieldFilter: FieldInfo => Boolean =
    if (isScalaObject) _.access.isStatic
    else if (isScala) x => !x.access.isStatic
    else _ => true

  def show = println(toString)
  override def toString = ClassFilePrettyPrinter(this)
}
