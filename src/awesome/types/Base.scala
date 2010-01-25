package awesome
package types

object Base {
  sealed abstract class Type(val tag: Char) extends types.Type { }
  
  val forClass: Map[JClass[_], Type] = Map(
    classOf[scala.Byte] -> Byte,
    classOf[scala.Short] -> Short,
    classOf[scala.Int] -> Int,
    classOf[scala.Long] -> Long,
    classOf[scala.Float] -> Float,
    classOf[scala.Double] -> Double,
    classOf[scala.Boolean] -> Boolean,
    classOf[scala.Char] -> Char,
    classOf[scala.Unit] -> Unit
  )
  val primitiveClasses = forClass.keysIterator.toList
  val primitiveNames = primitiveClasses map (_.getName)
  val boxedNames = primitiveNames map (_.capitalize) map {
    case "Char" => "Character"
    case "Int"  => "Integer"
    case x      => x
  }
  
  /** Primitives */
  case object Byte extends Type('B')
  case object Char extends Type('C')
  case object Double extends Type('D')
  case object Float extends Type('F')
  case object Int extends Type('I')
  case object Long extends Type('J')
  case object Short extends Type('S')
  case object Boolean extends Type('Z') { }

  /** Specials */
  case object Unit extends Type('V')
  case object Reference extends Type('L')
  case object Array extends Type('[') { }
  
  val numeric: Map[Char, Type] = Map(
    'B' -> Byte,
    'S' -> Short,
    'I' -> Int,
    'J' -> Long,
    'F' -> Float,
    'D' -> Double,
    'C' -> Char
  )
  val primitive = numeric   ++ Map('Z' -> Boolean)
  val anyval    = primitive ++ Map('V' -> Unit)
  val apply     = anyval    ++ Map('L' -> Reference, '[' -> Array)
}

