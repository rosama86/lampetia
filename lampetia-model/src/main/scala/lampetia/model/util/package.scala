package lampetia.model

import scala.util.{Failure, Success, Try}

/**
 * @author Hossam Karim
 */
package object util {

  val UUIDRegex = """^[a-f0-9]{8}-[a-f0-9]{4}-4[a-f0-9]{3}-[89aAbB][a-f0-9]{3}-[a-f0-9]{12}$""".r

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

    def isUUID = s match {
      case UUIDRegex(_*) => true
      case _             => false
    }
  }

  case object InvalidUUIDException extends Exception("Value is not a UUID")

  def parseUUID[A](candidate: String)(ctor: String => A): Try[A] = {
    val r = UUIDRegex
    candidate match {
      case r(_*) => Success(ctor(candidate))
      case _     => Failure(InvalidUUIDException)
    }
  }

}
