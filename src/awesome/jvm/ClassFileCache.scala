package awesome
package jvm

import attr.{ InnerClass, InnerClasses, Exceptions, ScalaSig }
import ClassFileParser.{ constant_pool, cp_class }

/** XXX dummy. */
class ClassFileCache { }

object ParsedCache {
  private[awesome] val parsedCache = new HashMap[String, Option[ParsedClassFile]]
  
  def apply(x: String): Option[ParsedClassFile] = parsedCache.getOrElseUpdate(x, PCF.one(x))
}

object ProcessedCache {
  private[awesome] val processedCache = new HashMap[String, Option[ClassFile]]

  def apply(x: String): Option[ClassFile] = processedCache.getOrElseUpdate(x, CF.one(x))
}