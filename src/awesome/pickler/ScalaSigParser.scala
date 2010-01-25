package awesome
package pickler

import parser._
import Constants._
import scala.util.parsing.combinator._
import scala.annotation.switch
 
class ScalaSigParser(val pool: Pool)
        extends StandardBinaryParsers
        with PicklerByteInterpretation
        with ImplicitConversions {
  
  def this() = this(new Pool())
  import pool._
  
  implicit def bytes2reader(xs: Seq[Byte]): ByteReader = new ByteReader(xs)

  val isEighthBitSet: Parser[Byte] = elem("isEighthBitSet", x => (x & 0x80) == 0)

  /** Natural numbers are terminated when some byte has its leftmost bit set. */
  lazy val natBytes = takeTo(isEighthBitSet)
  lazy val nat: Parser[Int] = natBytes ^^ toInt
  lazy val natLong: Parser[Long] = natBytes ^^ toLong
  
  /** Read flags, version 1 */
  lazy val symbolFlags   = natLong ^^ pickledToRawFlags ^^ Flags
  
  /** Read flags, version 2 */
  lazy val modifierFlags = natLong ~ nat ^^ { case x~y => pickledToRawFlags((x << 32) + y) } ^^ Flags
    
  lazy val nameRef          = nat ^^ NameRef
  lazy val symRef           = nat ^^ SymRef
  lazy val typeRef          = nat ^^ TypeRef
  lazy val lazyTypeRef      = nat ^^ LazyTypeRef
  lazy val lazyAliasRef     = nat ^^ LazyAliasRef
  lazy val thisTypeRef      = nat ^^ ThisTypeRef
  lazy val aliasRef         = nat ^^ AliasRef
  lazy val constantRef      = nat ^^ ConstantRef
  lazy val classRef         = nat ^^ ClassRef  
  lazy val constAnnotArgRef = nat ^^ ConstAnnotArgRef
  lazy val annotInfoRef     = nat ^^ AnnotInfoRef

  lazy val annotAssoc       = nameRef ~ constAnnotArgRef ^^ tuplify
  lazy val annotArgRef      = nat ^^ AnnotArgRef
  lazy val annotInfoBody    = typeRef ~ rep(annotArgRef) ~ rep(annotAssoc) ^^ AnnotInfoBody  
  
  // TODO
  def selfInAnnots = false              // settings.selfInAnnots.value
  // def isSymbolRef(index: Int) = false
  // def nameAtIndex(index: Int) = ""
  
  /** Sometimes the unpickler parses differently depending on whether the most recently
   *  read index contains a symbol reference.  So we embed that logic in the parser without
   *  evaluating it, then follow it up later.
   */
  def ifSymbolParser[T, U](p1: Int => T, p2: Int => U): Parser[(Option[T], U)] = nat >> { x =>
    if (isSymbolRef(x).get) nat ^^ (y => (Some(p1(x)), p2(y)))
    else success(None, p2(x))
  }
  
  /** This could use ifSymbolParser but this way we can reuse symInfo as-is.
   *  It encodes [defaultGetter_Ref] in the VALsym rule.
   */
  lazy val valsymPrefix = (
      nat ^? { case x if isSymbolRef(x).get => Some(SymRef(x)) }
    | success(None)
  )

  /** The ifSymbolParser encodes [privateWithin_Ref] in the SymbolInfo rule.
   */
  lazy val symInfo: Parser[SymInfo] =
    nameRef ~ symRef ~ symbolFlags ~ ifSymbolParser(SymRef, LazyTypeRef) ^^ {
      case name ~ owner ~ flags ~ ((privateWithin, info)) =>
        SymInfo(name, owner, flags, privateWithin, info)
    }
  
  lazy val nameInfo: Parser[NameInfo] = drain ^^ toUTF8String ^^ NameInfo
  
  /** The ifSymbolParser encodes [sym_Ref] in the ANNOTATEDtpe rule.
   */
  lazy val annotatedType =
    ifSymbolParser(SymRef, TypeRef) ~ rep(annotInfoRef) ^^ {
      case ((selfsym, underlying))~annots =>
        if (selfInAnnots || selfsym.isEmpty)
          AnnotatedType(selfsym, underlying, annots)
        else
          underlying
    }
  
  def mkExtRef(name: NameRef, owner: Option[SymRef]) = owner match {
    case Some(x)  => ExtRefWithOwner(name, x)
    case None     => ExtRef(name)
  }

  def read(tag: Int, xs: Seq[Byte]): Entry = {
    def mkConstant[T](x: T): Entry = Constant(tag, x)
    // def lazily[T <: Entry](p: Parser[T]): Entry = LazyEntry(tag, xs, this, p)

    /** These essentially save us some typing. */
    implicit def anyval2constant[T <: AnyVal](x: T): Entry = mkConstant(x)
    implicit def lift[T <: Entry](p: Parser[T]): Entry = p(xs) match {
      case Success(x, _)      => x
      case NoSuccess(msg, _)  => error("parse error in tag '%s': %s\nInput was: %s".format(Constants(tag), msg, xs.toString))
    }
    
    // Laziness in the trunk unpickler:
    //
    // CLASSsym:
    // if (readIndex != end) sym.typeOfThis = new LazyTypeRef(readNat())
    //
    // readSymbol:
    // symbols that were pickled with Pickler.writeSymInfo
    // which is everything except: EXTref | EXTMODCLASSref | NONEsym
    //
    // if (readIndex != end) new LazyTypeRefAndAlias(inforef, readNat())
    // else new LazyTypeRef(inforef))
    //
    

    val res: Entry = (tag: @switch) match {
      /** Names: 1-2 */
      case TERMname           => nameInfo ^^ TermName
      case TYPEname           => nameInfo ^^ TypeName
      
      /** Symbols: 3-10 */
      case NONEsym            => NoSymbol      
      case TYPEsym            => symInfo ^^ TypeSym
      case ALIASsym           => symInfo ^^ AliasSym      
      case CLASSsym           => symInfo ~ opt(lazyTypeRef) ^^ ClassSym
      // case CLASSsym           => lazily(symInfo ~ opt(thisTypeRef) ^^ ClassSym)
      case MODULEsym          => symInfo ^^ ModuleSym      
      // case VALsym             => lazily(valsymPrefix ~ symInfo ~ opt(aliasRef) ^^ ValSym)
      case VALsym             => valsymPrefix ~ symInfo ~ opt(lazyAliasRef) ^^ ValSym
      case EXTref             => nameRef ~ opt(symRef) ^^ mkExtRef
      case EXTMODCLASSref     => nameRef ~ opt(symRef) ^^ ExtModClassRef

      /* Types: 11-22, 42, 46-48 */
      case NOtpe              => NoType
      case NOPREFIXtpe        => NoPrefix
      case THIStpe            => symRef                           ^^ ThisType
      case SINGLEtpe          => typeRef ~ symRef                 ^^ SingleType
      case CONSTANTtpe        => constantRef                      ^^ ConstantType
      case TYPEREFtpe         => typeRef ~ symRef ~ rep(typeRef)  ^^ TypeRefType
      case TYPEBOUNDStpe      => typeRef ~ typeRef                ^^ TypeBoundsType
      case REFINEDtpe         => classRef ~ rep(typeRef)          ^^ RefinedType
      case CLASSINFOtpe       => classRef ~ rep(typeRef)          ^^ ClassInfoType
      case METHODtpe          => typeRef ~ rep(symRef)    ^^ swap ^^ MethodType
      case POLYtpe            => typeRef ~ rep(symRef)    ^^ swap ^^ PolyType
      case IMPLICITMETHODtpe  => typeRef ~ rep(symRef)    ^^ swap ^^ ImplicitMethodType
      case SUPERtpe           => typeRef ~ typeRef                ^^ SuperType
      case DEBRUIJNINDEXtpe   => nat ~ nat                        ^^ DeBruijnIndexType
      case EXISTENTIALtpe     => typeRef ~ rep(symRef)            ^^ ExistentialType
      case ANNOTATEDtpe       => annotatedType
      // case ANNOTATEDtpe       => lazily(annotatedType)
      
      /** Literals: 23-36 */
      case LITERALunit        => ()
      case LITERALboolean     => toInt(xs) != 0
      case LITERALbyte        => toInt(xs)
      case LITERALshort       => toInt(xs).toShort
      case LITERALchar        => toInt(xs).toChar
      case LITERALint         => toInt(xs)
      case LITERALlong        => toLong(xs)
      case LITERALfloat       => toFloat(xs)
      case LITERALdouble      => toDouble(xs)
      case LITERALnull        => ConstantNull
      case LITERALstring      => nameRef      ^^ mkConstant
      case LITERALclass       => typeRef      ^^ mkConstant
      case LITERALenum        => symRef       ^^ mkConstant
      
      /** Annotations and Misc: 40-41, 43-44, 50 */
      case SYMANNOT           => symRef ~ annotInfoBody   ^^ SymAnnotation
      case CHILDREN           => symRef ~ rep(symRef)     ^^ Children
      case ANNOTINFO          => annotInfoBody            ^^ AnnotInfo
      case ANNOTARGARRAY      => rep(constAnnotArgRef)    ^^ AnnotArgArray
      case MODIFIERS          => modifierFlags ~ nameRef  ^^ Modifiers
      
      /** Trees (still TODO): 49 */
      case TREE               => RawTree(xs.head, xs.tail)
      
      case _                  => error("Unknown Scala signature tag " + tag)
    }
    
    res withBook Bookkeeping(tag, xs)
  }
  
  lazy val entry    = nat ~ (nat >> bytes) ^^ read
  lazy val symtab   = nat >> (x => repN(x, entry))
  lazy val version  = nat ~ nat ^^ Version
  lazy val scalaSig = version ~ symtab ^^ { case v~entries => new ScalaSig(pool.createContext(entries), v) }    
  
  // lazy val myEntry = nat ~ (nat >> bytes) ^^ tuplify
  // lazy val sigBytes = version ~> (nat >> (x => repN(x, myEntry)))
   //   
   // (nat >> (nat ~ (nat >> bytes) ^^ { case a~b => ((a, b)) } ))
    
  def apply(xs: Seq[Byte]) = parseAll(scalaSig, xs)
  // def justBytes(xs: Seq[Byte]) = parseAll(sigBytes, xs)
}
// 
// scala> res3 diff res7
// res11: List[(Int, Seq[Byte])] = List((48,WrappedArray(77, 80, 89, 95, 100, 98)))
// 
// scala> res7 diff res3
// res12: List[(Int, Seq[Byte])] = List((48,WrappedArray(77, 98, 100, 89, 95, 80)))
// 
// *                  | 48 EXISTENTIALtpe len_Nat type_Ref {symbol_Ref}


object ScalaSigParser {
  def apply(xs: Seq[Byte]) = (new ScalaSigParser())(xs)
  // def justBytes(xs: Seq[Byte]) = (new ScalaSigParser()).justBytes(xs)
}