package lampetia.security.service

import lampetia.model.Email
import lampetia.sql.dialect.postgres.jdbc._
import lampetia.security.model._
import lampetia.security.model.SecurityModel._
import lampetia.security.model.SecuritySqlFormat._

/**
 * @author Hossam Karim
 */

trait UserService {

  def createUser(data: ProfileData): IO[User] = {
    val u = UserModel
    val p = ProfileModel
    val user = User(u.generate)
    val profile = Profile(p.generate, ProfileRef(user.id), data)
    (u += user)
     .flatMap(_ => p += profile)
     .transactionally
     .map(_ => user)
  }

  def createUserProfile(userId: UserId, data: ProfileData): IO[Profile] = {
    val p = ProfileModel
    val id = p.generate
    val profile = Profile(id, ProfileRef(userId), data)
    val action = p += profile
    action.map(_ => profile)
  }

  def findOrCreateUserProfile(userId: UserId, data: ProfileData): IO[Profile] = {
    val action = findProfileByProviderAndUserId(data.provider, data.providerUserId).flatMap {
      case Some(profile) => IO.pure(profile)
      case None          =>
        findProfileByProviderAndEmail(data.provider, data.email).flatMap {
          case Some(profile) => IO.pure(profile)
          case None          => createUserProfile(userId, data)
        }
    }
    action.transactionally
  }

  def findUserByProviderAndUserId(provider: AuthenticationProvider, providerUserId: ProviderUserId): IO[Option[User]] = {
    val u = UserModel
    val p = ProfileModel

    select(u.properties:_*)
    .from(u innerJoin p on (u.id === p.ref.userId))
    .where( (p.data.provider === provider.bind) and (p.data.providerUserId === providerUserId.bind) )
    .lifted
    .read[User]
    .map(_.headOption)
  }

  def findProfileByProviderAndEmail(provider: AuthenticationProvider, email: Email): IO[Option[Profile]] = {
    val p = ProfileModel

    select(p.properties:_*)
      .from(p)
      .where( (p.data.provider === provider.bind) and (p.data.email === email.bind) )
      .lifted
      .read[Profile]
      .map(_.headOption)
  }

  def findProfilesByEmail(email: Email): IO[Seq[Profile]] = {
    val p = ProfileModel

    select(p.properties:_*)
      .from(p)
      .where( p.data.email === email.bind)
      .lifted
      .read[Profile]
  }

  def findProfileByProviderAndUserId(provider: AuthenticationProvider, providerUserId: ProviderUserId): IO[Option[Profile]] = {
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



}
