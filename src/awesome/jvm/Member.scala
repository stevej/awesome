package awesome
package jvm

import attr._

trait Attributed {
  def attributes: List[Attribute]
  def findAttr[T](pf: Attribute =>? T): Option[T] = findMap(attributes)(pf)
} 

abstract class Member extends Attributed {
  type SigType <: Signature
  def createSignature: String => SigType

  def access: Flags
  def id: Ident
  def name = id.name
  
  def isPrivate = access.isPrivate
  def isPackagePrivate = access.isPackagePrivate
  
  def accessString = spaceSepString(
    if (isDeprecated) "@deprecated" else "",
    access.toString
  )

  lazy val sig: Option[SigType] = findAttr({ case x: SignatureAttr => createSignature(x.text) })

  def pkg: String = name.toPackage
  def unqualifiedName = name.toUnqualified
  def printableName = if (name == CLASS_INIT) "this" else name.toDecoded
  
  def isDeprecated = attributes exists (_.isDeprecated)
  def isSynthetic = access.isSynthetic || (attributes exists (_.isSynthetic))
}

abstract class FieldOrMethod extends Member {
  def descriptor: Descriptor
}
