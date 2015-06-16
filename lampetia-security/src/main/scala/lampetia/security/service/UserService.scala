package lampetia.security.service

import lampetia.model.Email
import lampetia.security.model._
import lampetia.security.module.SecurityModule
import lampetia.sql.dialect.postgres.jdbc._


/**
 * @author Hossam Karim
 */

trait UserService {

  import SecurityModule._

  protected def insertUser(user: User): IO[Int] = {
    val m = UserModel
    m.insert(m.id := user.id.bind)
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
      p.data.accountDetails := profile.data.accountDetails.bind.cast(Types.jsonb),
      p.data.accountState := profile.data.accountState.bind)
  }

  def createUser(data: ProfileData): IO[User] = {
    val u = UserModel
    val p = ProfileModel
    val user = User(u.generate)
    val profile = Profile(p.generate, ProfileRef(user.id), data)
     insertUser(user)
     .flatMap(_ => insertProfile(profile))
     .transactionally
     .map(_ => user)
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
    Q.update(p)
      .set(p.data.password, password.bind)
      .where((p.ref.userId === userId.bind) and (p.data.provider === UsernamePasswordProvider.bind))
      .lifted
      .write
  }



}

/*
object UserServiceTest extends App {

  import SecurityModule._

  implicit lazy val context: ConnectionSource = {
    val ds = new org.h2.jdbcx.JdbcDataSource
    ds.setUrl("jdbc:h2:mem:test;DB_CLOSE_DELAY=-1")
    connectionSource(ds)
  }

  val service = new UserService {}

  val data = ProfileData(
    UsernamePasswordProvider,
    ProviderUserId(""),
    ProviderResponse(PlayJson(Json.parse("[]"))),
    Email("user@acme.org"),
    Password("unsafe"),
    AccountDetails(PlayJson(Json.parse("[]"))),
    AccountActive)

  val f =
    UserModel.create
    .flatMap(_ => ProfileModel.create)
    .flatMap(_ => service.createUser(data))
    .transactionally
    .run

  f.onComplete {
    case Success(v) => println(v)
    case Failure(e) => println(e)
  }

  Await.ready(f, Duration.Inf)
  context.shutdown()
}
*/