package awesome
package pickler

import Constants._
import parser.ByteReader
import scala.reflect.NameTransformer

trait PoolwideMethods {
  self: Pool =>
  
  class PoolWithContext(val entries: List[Entry]) {
    val pool = self
    
    private val entryArray = entries.toArray
    private val names = new Array[String](entryArray.size)
    private val indices = 0 until entryArray.size
    private def entriesWhere(pf: Entry =>? Entry): List[Entry] = entries collect pf
    
    def isComplete(index: Int) = names(index) != null
    def isAllComplete = names forall (_ != null)
    def completeStats = "%d / %d".format(indices filter isComplete size, entryArray.size)
    
    def tagAt(index: Int)   = { entryArray(index).tag }
    def entryAt(index: Int) = { entryArray(index) }
    def nameAt(index: Int)  = { complete(index) ; Option(names(index)) }
    def isLiteral(e: Entry) = e.tag >= LITERALunit && e.tag <= LITERALenum

    def classInfos  = entriesWhere { case x: ClassInfoType => x }
    def classSyms   = entriesWhere { case x: ClassSym => x }
    def moduleSyms  = entriesWhere { case x: ModuleSym => x }
    def symbols     = entriesWhere { case x: SymEntry => x }
    def termNames   = entriesWhere { case x: TermName => x } uniqsortBy (_.toName)
    def trees       = entriesWhere { case x: Tree => x }
    def typeNames   = entriesWhere { case x: TypeName => x } uniqsortBy (_.toName)
    def types       = entriesWhere { case x: TypeEntry => x }

    def allNames    = termNames ::: typeNames  
    def literals    = entries filter isLiteral

    def methodTypes = entriesWhere {
      case x: MethodType => x
      case x: PolyType => x
      case x: ImplicitMethodType => x
    }
    def isRefinementSymbolEntry(index: Int) = entryAt(index) match {
      case ClassSym(SymInfo(x, _, _, _, _), _)  => x.toName == "<refinement>"
      case _                                    => false
    }
    def isSymbolEntry(index: Int) = {
      val tag = tagAt(index)
      firstSymTag <= tag && tag <= lastSymTag && (tag != CLASSsym || isRefinementSymbolEntry(index))
    }
    def isSymbolRef(index: Int) = {
      val tag = tagAt(index)
      firstSymTag <= tag && tag <= lastExtSymTag
    }

    def complete(index: Int): Boolean = { complete(index, Nil) }
    def complete(index: Int, seen: List[Int]): Boolean = {
      def seenStr = seen reverseMap entries map (_.safeToName) mkString " -> "

      if (seen contains index) {
        println("Cycle: " + seenStr)
        false
      }
      else if (!isComplete(index)) {
        val entry = entryArray(index)      
        entry.refs foreach (x => complete(x, index :: seen))

        if (entry.refs forall isComplete) {
          names(index) = entry.toName
          true
        }
        else false
      }
      else false
    }
    
    /* Constructor */
    indices foreach complete
  }  
}


/** The key to avoiding unresolvable cycles is not resolving:
 *    thistpe: Option[ThisTypeRef] in ClassSym  (class points at type, type points at class)
 *    entryInfo: SymInfo in ClassSym (can't get away with much in there)
 *    owner: SymRef in SymInfo  (obvious reasons)
 *
 *  That's enough to get Predef to complete anyway.
 */
class Pool() extends PoolwideMethods {
  implicit def refIndices(xs: List[Ref[_]]): List[Int] = xs map (_.index)
  def refidx(xs: Ref[_]*): List[Int] = xs.toList map (_.index)

  private var context: PoolWithContext = _
  def nameAt(index: Int) = Option(context) flatMap (_ nameAt index) orElse Some("???")
  def isSymbolRef(index: Int): Option[Boolean] = Option(context) map (_ isSymbolRef index) orElse Some(false)
  
  def createContext(entries: List[Entry]): PoolWithContext = {
    val res = new PoolWithContext(entries)
    context = res
    res
  }

  case class Flags(flags: Long) {
    override def toString = flagsToString(flags)
  }
  
  def toNames(xs: List[Entry]) = xs map (_.toName) mkString ", "

  case class Bookkeeping(tag: Int, bytes: Seq[Byte]) { }
  // , startIndex: Int) { }
    
  trait Entry {
    private var _book: Bookkeeping = null
    def book = Option(_book) getOrElse error("Bookkeeping not set in: " + this)
    def withBook(b: Bookkeeping): this.type = {
      _book = b
      this
    }
    def tag = book.tag
    def rawbytes = book.bytes
    // def startIndex = book.startIndex
    def tagName = if (tag == -1) "?" else Constants(tag)

    def process: Entry = this
    def isLazy = false
    def toName: String

    def safeToName: String = this match {
      case x: Named           => x.name.toName
      case x: SingletonEntry  => x.toName
      case x                  => tagName
    } 
    // def refs: List[Ref[_]]
    // def refidx = refs map (_.index)
    def refs: List[Int]
    
    override def toString = NameTransformer.decode(toName)
  }
  
  trait Owned extends Entry {
    def owner: SymRef
  }
  trait Named extends Entry {
    def name: NameRef
  }
  
  trait SingletonEntry extends Entry {
    // override def toName = toString
    override def refs = Nil
  }

  /** Entries which carry actual information in them */
  trait InfoEntry[T <: Info] extends Entry {
    def entryInfo: T
  }
  trait AnnotInfoEntry extends InfoEntry[AnnotInfoBody] {
    def entryInfo: AnnotInfoBody
    lazy val AnnotInfoBody(infoRef, args, assocs) = entryInfo    
    override def refs = entryInfo.refs
  }
  trait SymEntry extends InfoEntry[SymInfo] with Owned with Named {
    def entryInfo: SymInfo
    lazy val SymInfo(name, owner, flags, privateWithin, infoRef) = entryInfo
    
    override def toName = name.toName
    override def refs = entryInfo.refs
  }
  trait NameEntry extends InfoEntry[NameInfo] {
    def entryInfo: NameInfo
    lazy val NameInfo(name) = entryInfo
    
    override def toName = name
    override def refs = Nil
  }

  /** Entries which carry nothing but references to others */
  trait TypeEntry extends Entry {
  }
  trait AnnotArg extends Entry
  trait Tree extends Entry with AnnotArg
  trait ConstAnnotArg extends Entry

  case class RawTree(treeTag: Int, bytes: Seq[Byte]) extends Tree {
    override def toName = "%s".format(Constants(treeTag))
    override def refs = Nil
    // override def toString = "%s(%d bytes)".format(Constants.Tree(treeTag), bytes.size)
  }
  case class LazyEntry[T <: Entry](override val tag: Int, bytes: Seq[Byte], parser: ScalaSigParser, p: ScalaSigParser#Parser[T]) extends Entry {
    import parser._
    
    private var lazyEvaluation: Entry = null
    private def complete: Entry = {
      if (lazyEvaluation == null) {
        lazyEvaluation = p(new ByteReader(bytes)) match {
          case Success(x, _)      => x
          case NoSuccess(msg, _)  => error("Lazy evaluation failed on %s: '%s'".format(this, msg))
        }
      }
      
      lazyEvaluation
    }
  
    override def isLazy = true
    override def toName = complete.toName
    override def refs = complete.refs
  }

  case class TermName(entryInfo: NameInfo) extends NameEntry {
    // override def toName = "term " + super.toName
  } 
  case class TypeName(entryInfo: NameInfo) extends NameEntry {
    // override def toName = super.toName
  }
  case object NoSymbol extends SingletonEntry {
    override def toName = "NoSymbol"
  }
  case class TypeSym(entryInfo: SymInfo) extends SymEntry {
    // override def toName = "type " + super.toName
  }
  case class AliasSym(entryInfo: SymInfo) extends SymEntry {
    override def toName = "alias " + super.toName
  }
  // case class ClassSym(entryInfo: SymInfo, thistpe: Option[ThisTypeRef]) extends SymEntry {
  case class ClassSym(entryInfo: SymInfo, thistpe: Option[LazyTypeRef]) extends SymEntry {  
    // override def toName = "class " + super.toName
    override def refs = Nil
    // These need be commented out for toName to work as is
    // override def refs = super.refs ::: refIndices(thistpe.toList)
  }
  case class ModuleSym(entryInfo: SymInfo) extends SymEntry {
    // override def toName = "module " + super.toName
  }
  case class ValSym(defaultGetter: Option[SymRef], entryInfo: SymInfo, alias: Option[LazyAliasRef]) extends SymEntry {
    // override def toName = "val " + super.toName
    private def getterString = defaultGetter map (" = " + _) getOrElse ""
    override def toName = super.toName + getterString
    override def refs = super.refs ::: refIndices(List(defaultGetter, alias).flatten)
  }
  
  trait ExtEntry extends Entry with Named {
    def name: NameRef
    // def owner: Option[SymRef]

    // private def ownerString = owner match {
    //   case Some(s)  => ", owner = " + s.toName
    //   case None     => ""
    // }
    // override def toName = "Ext(" + name.toName + ownerString + ")"
    // override def toName = "Ext(" + name.toName + ")"
    override def toName = name.toName
    override def refs = refidx(name)
    // override def refs = refidx(name :: owner.toList : _*)
  }
  
  case class ExtRef(name: NameRef) extends ExtEntry
  case class ExtRefWithOwner(name: NameRef, owner: SymRef) extends ExtEntry with Owned {
    override def refs = refidx(name, owner)
  }

  case class ExtModClassRef(name: NameRef, owner: Option[SymRef]) extends ExtEntry {
    override def refs = refidx(name :: owner.toList : _*)
  }

  case object NoType extends TypeEntry with SingletonEntry {
    override def toName = "NoType"
  }
  case object NoPrefix extends TypeEntry with SingletonEntry {
    override def toName = "NoPrefix"
  }
  case class ThisType(name: SymRef) extends TypeEntry {
    override def toName = name.toName
    override def refs = refidx(name)
  }
  case class SingleType(prefix: TypeRef, sym: SymRef) extends TypeEntry {
    override def toName = sym.toName
    override def refs = refidx(prefix, sym)
  }
  case class ConstantType(value: ConstantRef) extends TypeEntry {
    override def toName = value.toName
    override def refs = refidx(value)
  }
  case class TypeRefType(prefix: TypeRef, sym: SymRef, args: List[TypeRef]) extends TypeEntry {
    private def prefixString: String = if (prefix.isNoPrefix || prefix.isPredef) "" else prefix.toName + "."
    private def argsString: String = if (args.isEmpty) "" else "[" + toNames(args) + "]"
    
    override def toName = prefixString + sym.toName + argsString
    // override def refs = refidx(prefix, sym)
    override def refs = refidx(prefix :: sym :: args : _*)
  }
  case class TypeBoundsType(lo: TypeRef, hi: TypeRef) extends TypeEntry {
    private def loString = if (lo.isNothing) "" else ">: " + lo.toName
    private def hiString = if (hi.isAny) "" else "<: " + hi.toName
    override def toName = spaceSepString(loString, hiString)
    override def refs = refidx(lo, hi)
  }
  case class RefinedType(clazz: ClassRef, parents: List[TypeRef]) extends TypeEntry {
    override def toName = "refinement of " + clazz.toName
    override def refs = refidx(clazz :: parents : _*)
  }
  case class ClassInfoType(clazz: ClassRef, parents: List[TypeRef]) extends TypeEntry {
    override def toName = "class info " + clazz.toName
    override def refs = refidx(clazz :: parents : _*)
  }
  case class MethodType(params: List[SymRef], resultType: TypeRef) extends TypeEntry {
    override def toName = "(%s) => %s".format(toNames(params), resultType.toName)
    override def refs = refidx(resultType :: params : _*)
  }
  case class PolyType(typeParams: List[SymRef], resultType: TypeRef) extends TypeEntry {
    private def typeParamString = if (typeParams.isEmpty) "" else "[" + toNames(typeParams) + "]"
    override def toName = typeParamString + " => " + resultType.toName
    override def refs = refidx(resultType :: typeParams : _*)
  }
  case class ImplicitMethodType(params: List[SymRef], resultType: TypeRef) extends TypeEntry {
    override def toName = "implicit (%s) => %s".format(toNames(params), resultType.toName)
    override def refs = refidx(resultType :: params : _*)
  }
  case class DeBruijnIndexType(level: Int, paramId: Int) extends TypeEntry {
    override def toName = "<param %d.%d>".format(level, paramId)
    override def refs = Nil
  }
  case class ExistentialType(underlying: TypeRef, quantified: List[SymRef]) extends TypeEntry {
    override def toName = "%s forSome { %s }".format(underlying.toName, toNames(quantified))
    override def refs = refidx(underlying :: quantified : _*)
  }
  case class SuperType(thistpe: TypeRef, supertpe: TypeRef) extends TypeEntry {
    override def toName = "%s.%s".format(thistpe.toName, supertpe.toName)
    override def refs = refidx(thistpe, supertpe)
  }
  case class AnnotatedType(selfsym: Option[SymRef], tp: TypeRef, staticAnnots: List[AnnotInfoRef]) extends TypeEntry {
    private def annotString = toNames(staticAnnots)
    override def toName = tp.toName + annotString
    override def refs = refidx(List[Ref[_]](tp) ::: staticAnnots ::: selfsym.toList: _*)
  }
  
  case class AnnotInfo(annot: AnnotInfoBody) extends ConstAnnotArg {
    lazy val AnnotInfoBody(infoRef, args, assocs) = annot
    private def assocString = 
      if (assocs.isEmpty) ""
      else (assocs map { case (k, v) => k.toName + "=" + v.toName }).mkString("(", ", ", ")")
      
    override def toName = "@%s%s%s".format(
      infoRef.toName,
      if (args.isEmpty) "" else toNames(args),
      assocString
    )    
    override def refs = annot.refs
  }
  case class AnnotArgArray(args: List[ConstAnnotArgRef]) extends ConstAnnotArg {
    override def toName = "AnnotationArray(%s)".format(toNames(args))
    override def refs = refIndices(args)
  }

  case class SymAnnotation(target: SymRef, annot: AnnotInfoBody) extends Entry {
    override def toName = "@(%s) on %s".format(annot, target)
    override def refs = target.index :: refidx(annot.refs: _*)
  }
  case class Children(target: SymRef, children: List[SymRef]) extends Entry {
    override def toName = "children of %s: %s".format(target.toName, toNames(children))
    override def refs = refIndices(target :: children)
  }

  trait ConstantValueEntry extends Entry with AnnotArg with ConstAnnotArg {
    override def refs = Nil
  }
  
  case class Constant[T](override val tag: Int, value: T) extends ConstantValueEntry {
    override def toName = value.toString
  }
  case object ConstantNull extends ConstantValueEntry with SingletonEntry {
    override def toName = "null"
  }

  case class Modifiers(flags: Flags, privateWithin: NameRef) extends Entry {
    override def toName = flags.toString
    override def refs = refidx(privateWithin)
  }

  trait Ref[+T <: Entry] extends Entry {
    def index: Int

    override def refs = Nil
    override def toName = nameAt(index) match {
      case Some(s)  => s
      case None     => "-> " + index
    }
  }
  object Ref {
    def unapply(other: Any) = other match {
      case x: Ref[_]  => Some((x.index))
      case _          => None
    }
  }

  trait LazyRef[T <: Entry, U <: Ref[T]] extends Ref[T] {
    def ref: U
    private var evaluated = false
    
    override def toName = 
      if (evaluated) process.toName
      else "<%d: lazy>".format(index)
      
    override lazy val process: Entry = {
      evaluated = true
      ref.process
    }
  }
  
  case class TypeRef(index: Int) extends Ref[TypeEntry] {
    def isNoPrefix = toName == NoPrefix.toName
    def isPredef = toName == "Predef"
    def isNothing = toName == "Nothing" || toName == "scala.Nothing"
    def isAny = toName == "Any" || toName == "scala.Any"
  }
  case class ThisTypeRef(index: Int) extends Ref[ThisType]
  case class SymRef(index: Int) extends Ref[SymEntry] {
    // def symEntry = lookup(index).asInstanceOf[SymEntry]
    def isNoSymbol = toName == "NoSymbol"
    // def path: String = {
    //   val prefix = if (owner.isNoSymbol) "" else owner.path + "."
    //   prefix + name.toName
    // }
  }
  case class AliasRef(index: Int) extends Ref[AliasSym]
  case class ClassRef(index: Int) extends Ref[ClassSym]
  case class TreeRef(index: Int) extends Ref[Tree]
  case class NameRef(index: Int) extends Ref[NameEntry]
  case class ConstantRef(index: Int) extends Ref[Constant[_]]
  case class AnnotInfoRef(index: Int) extends Ref[AnnotInfo]
  case class AnnotArgRef(index: Int) extends Ref[AnnotArg]
  case class ConstAnnotArgRef(index: Int) extends Ref[ConstAnnotArg] {}

  // case class LazyTypeRefAndAlias(index: Int, aliasIndex: Int) extends Ref[Entry]

  case class LazyAliasRef(index: Int) extends LazyRef[AliasSym, AliasRef] {
    override lazy val ref = AliasRef(index)
  }
  case class LazyTypeRef(index: Int) extends LazyRef[TypeEntry, TypeRef] {
    override lazy val ref = TypeRef(index)
  }

  /** Chunks of information contained in entries.  Not entries themselves. */

  trait Info {
    def refs: List[Ref[_]]
    def refidx: List[Int] = refs map (_.index)
  }

  case class SymInfo(
    name: NameRef,
    owner: SymRef,
    flags: Flags,
    privateWithin: Option[SymRef],
    infoRef: Ref[Entry]
  ) extends Info {
    
    override def refs = name :: owner :: privateWithin.toList
    // def refs = refidx(refNames: _*)
    // def refs = refidx(name, infoRef: _*)
      
    // def refs = refidx(List[Ref[_]](name) ::: List(infoRef) ::: privateWithin.toList : _*)
    // def refs = refIndices(List(name, owner, infoRef) ::: privateWithin.toList)
  }

  case class NameInfo(name: String) extends Info {
    def refs = Nil
    def qname: String = "\"" + name + "\""
  
    override def toString = name
  }

  case class AnnotInfoBody(
    infoRef: TypeRef,
    args: List[AnnotArgRef],
    assocs: List[(NameRef, ConstAnnotArgRef)]
  ) extends Info {
    
    type SomeRef = Ref[_ <: Entry]
    private lazy val assocRefs: List[SomeRef] = assocs flatMap { case (k, v) => List(k, v) }
    // private lazy val argRefs: List[Ref[_]] = args
    
    def refs: List[SomeRef] = (infoRef: SomeRef) :: (args: List[SomeRef]) ::: (assocRefs: List[SomeRef])
  }
}