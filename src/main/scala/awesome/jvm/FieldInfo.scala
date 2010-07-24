package awesome
package jvm

case class FieldInfo(
  owner: ClassInfo,
  access: Flags,
  id: Ident,
  descriptor: FieldDescriptor,
  attributes: List[Attribute]
) extends FieldOrMethod {
  
  type SigType = FieldSignature
  def createSignature = x => FieldSignature(x, id.toScalaString)
  
  private def valOrVar = if (access.isFinal) "val" else "var"
  private def constString = attributes find (_.isConstantValue) map (" = " + _) getOrElse ""
  
  override def toString = spaceSepString(
    accessString,
    "%s %s: %s".format(valOrVar, printableName, descriptor.decoded),
    constString
  )
}
