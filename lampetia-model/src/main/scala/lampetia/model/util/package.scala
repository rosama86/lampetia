package lampetia.model

/**
 * @author Hossam Karim
 */
package object util {
  implicit class StringEx(val s: String) extends AnyVal {

    def camelCase = s"${s.charAt(0).toLower}${s.substring(1)}"

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
  }
}
