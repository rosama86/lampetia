package lampetia.security.spray.route


import lampetia.security.module.SecurityModule
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

import scala.util.Try

/**
 * @author rhelal
 */
object Authenticator {

  import io.jsonwebtoken.{Jwts, SignatureAlgorithm}

  private val logger = LoggerFactory.getLogger("Authenticator")

  lazy val key: Array[Byte] = {
    val raw = SecurityModule.configuration.jwtKey
    raw.getBytes("UTF-8")
  }

  def claims = {
    val now = DateTime.now
    Jwts.claims()
      .setIssuer("https://www.nextechnology.me/")
      .setIssuedAt(now.toDate)
      .setExpiration(now.plusDays(30).toDate)
  }

  def compact(userId: String) =
    Jwts.builder().setClaims(claims).setSubject(userId).signWith(SignatureAlgorithm.HS256, key).compact()

  def userId(compactJwt: String): Try[String] = {

    Try(Jwts.parser().setSigningKey(key).parseClaimsJws(compactJwt).getBody.getSubject)
  }

  def main(aregs: Array[String]): Unit =
    println("token=" + Authenticator.compact("d0d60d34-1141-402a-8409-5dd295d50cf6"))

}
