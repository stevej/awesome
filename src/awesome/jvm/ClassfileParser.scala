package awesome
package jvm

import scala.util.parsing.combinator._
import scala.annotation.tailrec
import io.ByteCode
import parser._

/** In this model we hew very close to the JVM classfile specification
 *  and do no processing except that which can be done with immediately
 *  available data.
 */
object ClassFileParser extends JVMParsers {
  // TODO - see
  // http://www.openjdk.org/projects/jigsaw/doc/classfile.html
  
  trait named_info {
    def name_index: Int
  }

  case class field_info(
    access_flags: Int,
    name_index: Int,
    descriptor_index: Int,
    attributes: List[field_attribute_info]  // length: u2
  ) extends named_info { }
  
  case class method_info(
    access_flags: Int,
    name_index: Int,
    descriptor_index: Int,
    attributes: List[method_attribute_info]  // length: u2
  ) extends named_info { }

  trait attribute_info extends named_info {
    def info: Seq[Byte]
  }

  object attribute_info {
    def unapply(x: Any) = x match {
      case x: attribute_info  => Some((x.name_index, x.info))
      case _                  => None
    }
  }

  case class method_attribute_info(name_index: Int, info: Seq[Byte]) extends attribute_info { }
  case class field_attribute_info(name_index: Int, info: Seq[Byte]) extends attribute_info { }
  case class class_attribute_info(name_index: Int, info: Seq[Byte]) extends attribute_info { }
  case class code_attribute_info(name_index: Int, info: Seq[Byte]) extends attribute_info { }
  
  /** This is slightly out of place */
  case class inner_class_info(inner_class: Int, outer_class: Int, inner_name: Int, access_flags: Int) { }
  
  val one_attribute = u2 ~ (u4 >> bytes)  
  val parse_interface_info = u2
  val parse_field_info = u2 ~ u2 ~ u2 ~ collect(one_attribute ^^ field_attribute_info) ^^ field_info
  val parse_method_info = u2 ~ u2 ~ u2 ~ collect(one_attribute ^^ method_attribute_info) ^^ method_info
  
  sealed trait cp_info {
    type T
    def value: T
    def size = 1
  }
  trait cp_value extends cp_info { }
  /** Long and Double take 2 spots in the constant pool. */
  trait cp_value2 extends cp_value {
    override def size = 2
  }
  trait cp_info_index extends cp_info {
    type T = Int
  }
  trait cp_info_index2 extends cp_info {
    type T = (Int, Int)
  }
  
  object cp_value {
    def unapply(x: Any) = x match {
      case x: cp_value  => Some(x.value)
      case _            => None
    }
  }

  case class cp_utf8(value: String) extends cp_value { type T = String }
  case class cp_int(value: Int) extends cp_value { type T = Int }
  case class cp_float(value: Float) extends cp_value { type T = Float }
  case class cp_long(value: Long) extends cp_value2 { type T = Long }
  case class cp_double(value: Double) extends cp_value2 { type T = Double }
  case class cp_string(value: Int) extends cp_value with cp_info_index
  case class cp_class(value: Int) extends cp_info_index
  case class cp_field_ref(value: (Int, Int)) extends cp_info_index2
  case class cp_method_ref(value: (Int, Int)) extends cp_info_index2
  case class cp_interface_method_ref(value: (Int, Int)) extends cp_info_index2
  case class cp_name_and_type_ref(value: (Int, Int)) extends cp_info_index2
  
  class constant_pool(private val entries: Array[cp_info]) extends Iterable[(Int, cp_info)] {
    def infos = iterator map (_._2)
    def iterator = {
      def useIndex(x: Int) = 
        this(x) != null && this(x).size == 1 // hop the double-wide entries
      
      1 to entries.size filter useIndex map (x => (x, this(x))) iterator
    }
    
    def apply(index: Int) = index match {
      case 0    => error("parse error, requested invalid cp entry.\n" + this)
      case idx  => entries(index - 1)
    }
    
    private def entryString(pair: (Int, cp_info)) = "const #%d = %s".format(pair._1, pair._2)
    override def toString = iterator map entryString mkString("", "\n", "\n")
  }
  
  lazy val indexPair = u2 ~ u2 ^^ { case x~y => ((x, y)) }
  
  def categorize_cp_info(tag: Int): Parser[cp_info] = tag match {
    case CONSTANT_Utf8                => u2 >> bytes ^^ toUTF8String ^^ cp_utf8
    case CONSTANT_Unicode             => error("unimplemented")
    case CONSTANT_Integer             => u4 ^^ cp_int
    case CONSTANT_Float               => u4f ^^ cp_float
    case CONSTANT_Long                => u8 ^^ cp_long
    case CONSTANT_Double              => u8d ^^ cp_double
    case CONSTANT_Class               => u2 ^^ cp_class
    case CONSTANT_String              => u2 ^^ cp_string
    case CONSTANT_Fieldref            => indexPair ^^ cp_field_ref
    case CONSTANT_Methodref           => indexPair ^^ cp_method_ref
    case CONSTANT_InterfaceMethodref  => indexPair ^^ cp_interface_method_ref
    case CONSTANT_NameAndType         => indexPair ^^ cp_name_and_type_ref
  }
    
  lazy val parse_cp_info: Parser[cp_info] = u1 >> categorize_cp_info
  
  def collect_cp_info(size: Int) = new Parser[constant_pool] {
    private val pool = new Array[cp_info](size - 1)
    private var index = 0
    
    @tailrec 
    final def apply(in: Input): ParseResult[constant_pool] = {
      if (index >= (size - 1)) Success(new constant_pool(pool), in)
      else parse_cp_info(in) match {
        case Success(entry, rest) =>
          pool(index) = entry
          index += entry.size
          apply(rest)
        case ns: NoSuccess  => return ns
      }
    }
  }
  
  val magicNumber = u4 ^? ({ case JAVA_MAGIC => true }, _ => "bad magic number")
  val version = indexPair
  val access_flags, this_class, super_class = u2
  
  lazy val classFile = (
    magicNumber ~> 
    version ~
    (u2 >> collect_cp_info ^^ ParsedPool) ~
    access_flags ~ 
    this_class ~ 
    super_class ~
    collect(parse_interface_info) ~
    collect(parse_field_info) ~
    collect(parse_method_info) ~
    collect(one_attribute ^^ class_attribute_info)
  ) ^^ {
    case a~b~c~d~e~f~g~h~i => new ParsedClassFile(a, b, c, d, e, f, g, h, i)
  }
  
  def opt(name: String): Option[ParsedClassFile] = apply(name).toOption
  def opt(xs: Seq[Byte]): Option[ParsedClassFile] = apply(xs).toOption
  
  def apply(id: Ident) = cache(id.name)
  def apply(clazz: JClass[_]): ParseResult[ParsedClassFile] = cache(clazz)
  def apply(name: String): ParseResult[ParsedClassFile] = cache(name)
  def apply(xs: Seq[Byte]): ParseResult[ParsedClassFile] = cache(xs)
  
  lazy val cache = new io.Cacher {
    type ResultType = ParseResult[ParsedClassFile]
    
    def apply(name: String)   = getOrElseUpdate(name, apply(ByteCode(name)))
    def apply(xs: Seq[Byte])  = getOrElseUpdate(md5(xs), phrase(classFile)(new ByteReader(xs)))
  }
}
