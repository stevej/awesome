package awesome

import scala.reflect.NameTransformer
import types.Base

sealed trait Ident {
  /** "name" this entity with zero processing: exactly as we get it. */
  def name: String
  /** any given type arguments */
  def typeArgs: List[String]
  /** canonical forms */
  lazy val (pkgSegments, qualifiers, unqualifiedName) = {
    val segs = toInternal split '/'
    val last = segs.last

    if (last contains "$$")
      (segs.init, Nil, last)
    else {
      val isObject = last endsWith "$"
      val segs2 = last split '$'
      val unqname = segs2.last + (if (isObject) "$" else "")
      
      (segs.init, segs2.init, unqname)
    }
  }
  
  /** relative to package or containing class */
  // def asSeenBy(other: Ident): Ident =
  
  def toRawString: String = name
  def toScalaString: String = Ident.asScala(name, typeArgs)

  def isSetter = name endsWith "_$eq"
  def isExpanded = name contains "$$"
  def containsDollarSign = name contains "$"
  def isSerializingMethod = name == "readObject" || name == "writeObject"
  def isConstructor = name == CLASS_INIT
  def isStaticConstructor = name == STATIC_INIT
  def isAnyConstructor = isConstructor || isStaticConstructor
  
  def toEncoded = NameTransformer encode name
  def toDecoded = NameTransformer decode name
  def toInternal = name.replace('.', '/')
  def toExternal = name.replace('/', '.')
  
  def segments = toInternal split '/'
  def toUnqualified = toInternal split '/' last
  def toPackage = (toInternal split '/').init mkString "."
  def toLastPackage = toPackage split '.' last
  def toCompanion = Ident(
    if (name endsWith "$") name dropRight 1 else name + "$"
  )
  
  def clazz = forName(toExternal)
  def clazzOpt = Option(clazz)
  
  def isObject = toExternal == "java.lang.Object" || toExternal == "AnyRef"
  def isTuple = toExternal startsWith "scala.Tuple"
  def isFunction = toExternal startsWith "scala.Function"
  def isPartialFunction = toExternal startsWith "scala.PartialFunction"
  def isCollection = toExternal startsWith "scala.collection."
  
  override def toString = toScalaString
  
  override def hashCode = toExternal.hashCode
  override def equals(other: Any) = other match {
    case x: Ident => this.toExternal == x.toExternal
    case _        => false
  }
}

object Ident extends (String => Ident) {
  val stripPrefixes = List("java.lang.", "scala.math.", "scala.")
  // official
  val aliases = Map(
    "scala.collection.immutable.List" -> "List",
    "scala.collection.Traversable" -> "Traversable",
    "scala.collection.Iterable" -> "Iterable",
    "scala.collection.Seq" -> "Seq",
    "scala.collection.IndexedSeq" -> "IndexedSeq",
    "scala.collection.Iterator" -> "Iterator",
    "scala.collection.immutable.::" -> "::",
    "scala.collection.immutable.Nil" -> "Nil",
    "scala.collection.immutable.Stream" -> "Stream",
    "scala.collection.immutable.Vector" -> "Vector",
    "scala.collection.mutable.StringBuilder" -> "StringBuilder",
    "scala.collection.immutable.Range" -> "Range",
    "scala.collection.generic.CanBuildFrom" -> "CanBuildFrom"
  ) ++
  // just for noise reduction
  Map(
    "scala.collection.mutable.WrappedArray" -> "WrappedArray" 
  )
  
  val regexps = Map(
    """^scala\.collection\.mutable""" -> "mutable",
    """^scala\.collection\.immutable""" -> "immutable",
    """^scala\.collection\.generic""" -> "generic",
    """^java\.lang\.""" -> "",
    """^scala\.math\.""" -> "",
    """^scala\.""" -> ""
  )
  
  def asScala(givenName: String, typeArgs: List[String]): String = {
    val name = NameTransformer decode givenName.replace('/', '.')
    def tpString = tparamsToString(typeArgs)
    def tp(x: Int) = typeArgs(x)

    def isObject = name == "java.lang.Object" || name == "AnyRef"
    def isTuple = name startsWith "scala.Tuple"
    def isFunction = name startsWith "scala.Function"
    def isPartialFunction = name startsWith "scala.PartialFunction"
    def isCollection = name startsWith "scala.collection."
    def isBoxed = (name startsWith "java.lang.") && (Base.boxedNames contains name.stripPrefix("java.lang."))
    
    if (isObject) "AnyRef"
    else if (isTuple && typeArgs.nonEmpty) paramsToString(typeArgs)
    else if (isFunction) typeArgs match {
      case Nil          => name
      case List(t1)     => "() => " + t1
      case List(t1, t2) => "%s => %s".format(t1, t2)
      case xs           => "%s => %s".format(paramsToString(xs.init), xs.last)
    }    
    else if (isPartialFunction && typeArgs.size == 2) "%s =>? %s".format(typeArgs: _*)
    else if (aliases contains name) aliases(name) + tpString
    else if (isBoxed) name.replaceAll("""^java\.lang\.""", "jl.")
    else {
      val res = name + tpString
      for ((re, replacement) <- regexps) {
        val res2 = res.replaceFirst(re, replacement)
        if (res2 != res)
          return res2
      }
      res
    }
  }
  
  object initIdent extends SimpleIdent(CLASS_INIT, Nil)
  object clinitIdent extends SimpleIdent(STATIC_INIT, Nil)
  
  private def parseTParams(s: String): List[String] =
    (s filterNot ("<>[]" contains _) split ',').toList map (_.trim) 
    
  def apply(name: String): Ident = apply(name, Nil)
  def apply(name: String, typeArgs: List[String]): Ident =
    if (name == CLASS_INIT) initIdent
    else if (name == STATIC_INIT) clinitIdent
    else new SimpleIdent(name, typeArgs)

  def unapply(x: Any) = x match {
    case x: Ident   => Some((x.name, x.typeArgs))
    case _          => None
  }
}

class SimpleIdent(val name: String, val typeArgs: List[String]) extends Ident { }
