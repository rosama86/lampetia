package lampetia.security.service

import lampetia.model.Email
import lampetia.security.model._



/**
 * @author Hossam Karim
 */

trait UserService {

  import lampetia.security.module.SecurityModule.sql._

  protected def insertUser(user: User): IO[Int] = {
    val m = UserModel
    m.insert(m.id := user.id.bind, m.accountState := user.accountState.bind)
  }

  protected def insertProfile(profile: Profile): IO[Int] = {
    val p = ProfileModel
    p.insert(
      p.id := profile.id.bind,
      p.ref.userId := profile.ref.userId.bind,
      p.data.provider := profile.data.provider.bind,
      p.data.providerUserId := profile.data.providerUserId.bind,
      p.data.providerResponse := profile.data.providerResponse.bind.cast(Types.jsonb),
      p.data.email := profile.data.email.bind,
      p.data.password := profile.data.password.bind,
      p.data.accountDetails := profile.data.accountDetails.bind.cast(Types.jsonb))
  }

  def createUser(data: ProfileData): IO[User] = {
    val u = UserModel
    val p = ProfileModel
    val user = User(u.generate, AccountActive)
    val profile = Profile(p.generate, ProfileRef(user.id), data)
     insertUser(user)
     .flatMap(_ => insertProfile(profile))
     .transactionally
     .map(_ => user)
  }

  def findOne(id: UserId): IO[Option[User]] = {
    val u = UserModel
    select(u.properties: _*)
      .from(u.schemaPrefixed)
      .where(u.id === id.bind)
      .lifted
      .read[User]
      .map(_.headOption)
  }

  def createUserProfile(userId: UserId, data: ProfileData): IO[Profile] = {
    val p = ProfileModel
    val id = p.generate
    val profile = Profile(id, ProfileRef(userId), data)
    val action = insertProfile(profile)
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
    p.update(p.data.password := Some(password).bind)(
      (p.ref.userId === userId.bind) and (p.data.provider === UsernamePasswordProvider.bind)
    )
  }

  def suspendAccount(userId: UserId): IO[Int] = {
    val u = UserModel
    u.update(u.accountState := AccountSuspended.bind)(u.id === userId.bind)
  }



}

