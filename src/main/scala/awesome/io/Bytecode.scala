package awesome
package io

object ByteCode {
  def apply(name: String): Array[Byte] =
    name.stripSuffix(".class").clazzOpt map apply getOrElse Array()

  def apply(clazz: JClass[_]): Array[Byte] = {
    val name = clazz.getName
    val subPath = name.substring(name.lastIndexOf('.') + 1) + ".class"
    val in = clazz.getResourceAsStream(subPath)
    if (in == null) Array()
    else try {
      var rest = in.available()
      val bytes = new Array[Byte](rest)
      while (rest > 0) {
        val res = in.read(bytes, bytes.length - rest, rest)
        if (res == -1) throw new IOException("read error")
        rest -= res
      }
      bytes
    }
    finally in.close()
  }
}