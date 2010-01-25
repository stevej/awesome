package awesome
package io

abstract class Cacher {
  type ResultType
  
  def apply(name: String): ResultType
  def apply(xs: Seq[Byte]): ResultType
  def apply(clazz: JClass[_]): ResultType  = apply(clazz.getName)
  
  val nameCache = new HashMap[String, ResultType]
  
  private var fills = 0
  private var hits = 0
  def stats = "%d fills, %d hits".format(fills, hits)
  
  def md5(xs: Seq[Byte]): String = {
    val md5er = java.security.MessageDigest.getInstance("MD5")
    md5er.update(xs.toArray)      
    BigInt(1, md5er.digest()) toString 16
  }
  
  protected def getOrElseUpdate(s: String, body: => ResultType) = {
    hits += 1
    nameCache.getOrElseUpdate(s, {
      hits -= 1
      fills += 1
      body
    })
  }
}
