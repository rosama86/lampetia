package lampetia.extensions

/**
 * @author Hossam Karim
 */

object Strings {


  implicit class StringEx(s: String) {

    def camelCase = s"${s.charAt(0).toLower}${s.substring(1)}"

    def CamelCase = s"${s.charAt(0).toUpper}${s.substring(1)}"

    def snakeCase = {
      val r = """((?<=[a-z0-9])[A-Z]|(?!^)[A-Z](?=[a-z]))""".r
      r.replaceAllIn(s, "_$1").toLowerCase
    }

    def lispCase = {
      val r = """((?<=[a-z0-9])[A-Z]|(?!^)[A-Z](?=[a-z]))""".r
      r.replaceAllIn(s, "-$1").toLowerCase
    }

    def qoute = s""""$s""""

    def backqoute = s"`$s`"

    def dollar = s"$${$s}"

    def packageToDir = s.replaceAll("\\.", "/")

    def toJavaIdentifier = {
        s.split("-").map(ss => s"${ss.charAt(0).toUpper}${ss.substring(1)}").reduceLeft(  _ + _)
    }
  }
}
