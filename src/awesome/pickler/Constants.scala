package awesome
package pickler

/** Cut/paste of PickleFormat Dec 30 2009. */
object Constants {
  val MajorVersion = 5
  val MinorVersion = 0

  final val TERMname = 1
  final val TYPEname = 2
  final val NONEsym = 3
  final val TYPEsym = 4
  final val ALIASsym = 5
  final val CLASSsym = 6
  final val MODULEsym = 7
  final val VALsym = 8
  final val EXTref = 9
  final val EXTMODCLASSref = 10
  final val NOtpe = 11
  final val NOPREFIXtpe = 12
  final val THIStpe = 13
  final val SINGLEtpe = 14
  final val CONSTANTtpe = 15
  final val TYPEREFtpe = 16
  final val TYPEBOUNDStpe = 17
  final val REFINEDtpe = 18
  final val CLASSINFOtpe = 19
  final val METHODtpe = 20
  final val POLYtpe = 21
  final val IMPLICITMETHODtpe = 22

  // literals
  final val LITERAL = 23   // base line for literals
  final val LITERALunit = 24
  final val LITERALboolean = 25
  final val LITERALbyte = 26
  final val LITERALshort = 27
  final val LITERALchar = 28
  final val LITERALint = 29
  final val LITERALlong = 30
  final val LITERALfloat = 31
  final val LITERALdouble = 32
  final val LITERALstring = 33
  final val LITERALnull = 34
  final val LITERALclass = 35
  final val LITERALenum = 36

  final val SYMANNOT = 40
  final val CHILDREN = 41
  final val ANNOTATEDtpe = 42
  final val ANNOTINFO = 43
  final val ANNOTARGARRAY = 44
  // 45: this space left blank
  final val SUPERtpe = 46
  final val DEBRUIJNINDEXtpe = 47
  final val EXISTENTIALtpe = 48
  final val TREE = 49             // prefix code that means a tree is coming
  final val MODIFIERS = 50

  final val firstSymTag = NONEsym
  final val lastSymTag = VALsym
  final val lastExtSymTag = EXTMODCLASSref

  // Following TREE
  object Tree {
    final val EMPTYtree = 1
    final val PACKAGEtree = 2
    final val CLASStree = 3
    final val MODULEtree = 4
    final val VALDEFtree = 5
    final val DEFDEFtree = 6
    final val TYPEDEFtree = 7
    final val LABELtree = 8
    final val IMPORTtree = 9
    final val DOCDEFtree = 11
    final val TEMPLATEtree = 12
    final val BLOCKtree = 13
    final val CASEtree = 14
    // final val SEQUENCEtree = 15        // removed
    final val ALTERNATIVEtree = 16
    final val STARtree = 17
    final val BINDtree = 18
    final val UNAPPLYtree = 19
    final val ARRAYVALUEtree = 20
    final val FUNCTIONtree = 21
    final val ASSIGNtree = 22
    final val IFtree = 23
    final val MATCHtree = 24
    final val RETURNtree = 25
    final val TREtree = 26
    final val THROWtree = 27
    final val NEWtree = 28
    final val TYPEDtree = 29
    final val TYPEAPPLYtree = 30
    final val APPLYtree = 31
    final val APPLYDYNAMICtree = 32
    final val SUPERtree = 33
    final val THIStree = 34
    final val SELECTtree = 35
    final val IDENTtree = 36
    final val LITERALtree = 37
    final val TYPEtree = 38
    final val ANNOTATEDtree = 39
    final val SINGLETONTYPEtree = 40
    final val SELECTFROMTYPEtree = 41
    final val COMPOUNDTYPEtree = 42
    final val APPLIEDTYPEtree = 43
    final val TYPEBOUNDStree = 44
    final val EXISTENTIALTYPEtree = 45
    
    // def pp(x: String) = 
    // printf("case %s%s => %s\n".format(x, " " * (20 - x.length), "\"" + x.stripSuffix("tree") + "\""))
    def apply(tree: Int): String = tree match {
      case EMPTYtree            => "EMPTY"
      case PACKAGEtree          => "PACKAGE"
      case CLASStree            => "CLASS"
      case MODULEtree           => "MODULE"
      case VALDEFtree           => "VALDEF"
      case DEFDEFtree           => "DEFDEF"
      case TYPEDEFtree          => "TYPEDEF"
      case LABELtree            => "LABEL"
      case IMPORTtree           => "IMPORT"
      case DOCDEFtree           => "DOCDEF"
      case TEMPLATEtree         => "TEMPLATE"
      case BLOCKtree            => "BLOCK"
      case CASEtree             => "CASE"
      case ALTERNATIVEtree      => "ALTERNATIVE"
      case STARtree             => "STAR"
      case BINDtree             => "BIND"
      case UNAPPLYtree          => "UNAPPLY"
      case ARRAYVALUEtree       => "ARRAYVALUE"
      case FUNCTIONtree         => "FUNCTION"
      case ASSIGNtree           => "ASSIGN"
      case IFtree               => "IF"
      case MATCHtree            => "MATCH"
      case RETURNtree           => "RETURN"
      case TREtree              => "TRE"
      case THROWtree            => "THROW"
      case NEWtree              => "NEW"
      case TYPEDtree            => "TYPED"
      case TYPEAPPLYtree        => "TYPEAPPLY"
      case APPLYtree            => "APPLY"
      case APPLYDYNAMICtree     => "APPLYDYNAMIC"
      case SUPERtree            => "SUPER"
      case THIStree             => "THIS"
      case SELECTtree           => "SELECT"
      case IDENTtree            => "IDENT"
      case LITERALtree          => "LITERAL"
      case TYPEtree             => "TYPE"
      case ANNOTATEDtree        => "ANNOTATED"
      case SINGLETONTYPEtree    => "SINGLETONTYPE"
      case SELECTFROMTYPEtree   => "SELECTFROMTYPE"
      case COMPOUNDTYPEtree     => "COMPOUNDTYPE"
      case APPLIEDTYPEtree      => "APPLIEDTYPE"
      case TYPEBOUNDStree       => "TYPEBOUNDS"
      case EXISTENTIALTYPEtree  => "EXISTENTIALTYPE"
    }
  }
  
  def apply(tag: Int): String = tag match {
    case TERMname       => "TERMname"
    case TYPEname       => "TYPEname"
    case NONEsym        => "NONEsym"
    case TYPEsym        => "TYPEsym"
    case ALIASsym       => "ALIASsym"
    case CLASSsym       => "CLASSsym"
    case MODULEsym      => "MODULEsym"
    case VALsym         => "VALsym"
    case EXTref         => "EXTref"
    case EXTMODCLASSref => "EXTMODCLASSref"
    case NOtpe          => "NOtpe"
    case NOPREFIXtpe    => "NOPREFIXtpe"
    case THIStpe        => "THIStpe"
    case SINGLEtpe      => "SINGLEtpe"
    case CONSTANTtpe    => "CONSTANTtpe"
    case TYPEREFtpe     => "TYPEREFtpe"
    case TYPEBOUNDStpe  => "TYPEBOUNDStpe"
    case REFINEDtpe     => "REFINEDtpe"
    case CLASSINFOtpe   => "CLASSINFOtpe"
    case METHODtpe      => "METHODtpe"
    case POLYtpe        => "POLYtpe"
    case IMPLICITMETHODtpe => "IMPLICITMETHODtpe"
    case LITERALunit    => "LITERALunit"
    case LITERALboolean => "LITERALboolean"
    case LITERALbyte    => "LITERALbyte"
    case LITERALshort   => "LITERALshort"
    case LITERALchar    => "LITERALchar"
    case LITERALint     => "LITERALint"
    case LITERALlong    => "LITERALlong"
    case LITERALfloat   => "LITERALfloat"
    case LITERALdouble  => "LITERALdouble"
    case LITERALstring  => "LITERALstring"
    case LITERALnull    => "LITERALnull"
    case LITERALclass   => "LITERALclass"
    case LITERALenum    => "LITERALenum"
    case SYMANNOT       => "SYMANNOT"
    case CHILDREN       => "CHILDREN"
    case ANNOTATEDtpe   => "ANNOTATEDtpe"
    case ANNOTINFO      => "ANNOTINFO"
    case ANNOTARGARRAY  => "ANNOTARGARRAY"
    case SUPERtpe       => "SUPERtpe"
    case DEBRUIJNINDEXtpe => "DEBRUIJNINDEXtpe"
    case EXISTENTIALtpe => "EXISTENTIALtpe"
    case TREE           => "TREE"
    case MODIFIERS      => "MODIFIERS"
    case _              => "***BAD TAG***(" + tag + ")"
  }
  
  //The following two are no longer accurate, because ANNOTATEDtpe,
  //SUPERtpe, ... are not in the same range as the other types
  //final val firstTypeTag = NOtpe
  //final val lastTypeTag = POLYtpe
}