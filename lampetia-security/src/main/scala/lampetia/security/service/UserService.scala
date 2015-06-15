package lampetia.security.service

import lampetia.sql.dialect.postgres.jdbc._
import lampetia.security.model._
import lampetia.security.model.SecurityModel._
import lampetia.security.model.SecuritySqlFormat._

/**
 * @author Hossam Karim
 */

trait UserService {

  def findUserByProviderIdAndUserId(provider: AuthenticationProvider, providerUserId: ProviderUserId): IO[Option[User]] = {
    val u = UserModel
    val p = ProfileModel

    select(u.properties:_*)
    .from(u innerJoin p on (u.id === p.ref.userId))
    .where( (p.data.provider === provider.bind) and (p.data.providerUserId === providerUserId.bind) )
    .lifted
    .read[User]
    .map(_.headOption)
  }

  def findProfileByProviderIdAndUserId(provider: AuthenticationProvider, providerUserId: ProviderUserId): IO[Option[Profile]] = {
    val p = ProfileModel
    select(p.properties:_*)
      .from(p)
      .where( (p.data.provider === provider.bind) and (p.data.providerUserId === providerUserId.bind) )
      .lifted
      .read[Profile]
      .map(_.headOption)
  }

  def findProfilesByUserId(userId: UserId): IO[Seq[Profile]] = {
    val p = ProfileModel
    select(p.properties:_*)
      .from(p)
      .where( p.ref.userId === userId.bind )
      .lifted
      .read[Profile]
  }

  def updatePassword(userId: UserId, password: Password): IO[Int] = {
    val p = ProfileModel
    Q.update(p)
      .set(p.data.password, password.bind)
      .where((p.ref.userId === userId.bind) and (p.data.provider === UsernamePasswordProvider.bind))
      .lifted
      .write
  }

  def addProfile(profile: Profile): IO[Int] = ProfileModel += profile




}
