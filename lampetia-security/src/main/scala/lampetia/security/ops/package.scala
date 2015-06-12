package lampetia.security

import lampetia.model.{ResourceId, HasId}
import lampetia.security.model.{Permission, SubjectId, SecureModel}
import lampetia.sql.ast.{Operand, Operator, QueryNodeBuilder}
import lampetia.sql.dialect.postgres._

/**
 * @author Hossam Karim
 */
package object ops {

  /*case class WithAuthority[E, Id](principle: SubjectId, permission: Permission, secureModel: SecureModel[E] with HasId[E, Id])

  implicit class LiftSecureModel[E, Id](val secureModel: SecureModel[E] with HasId[E, Id]) extends AnyVal {
    def authorize(principle: SubjectId, permission: Permission): WithAuthority[E, Id] =
      WithAuthority(principle, permission, secureModel)
  }

  trait SecureFind[E] extends Any { ms: ModelSchema[E] =>

    def find(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).lifted.read[E]

    def find[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).where(filter).lifted.read[E]

    def findOne[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Option[E]] =
      find(filter).map(_.headOption)

  }

  implicit class SecureModelOps[E, Id](val sm: WithAuthority[E, Id]) extends AnyVal {
    def model = sm.secureModel
    def permitted(principle: SubjectId, resource: Operand, permission: Permission) =
      function("permitted", principle.bind, resource, permission.bind)

    def find(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*)
      .from(model)
      .where(permitted(sm.principle, model.id, sm.permission))
      .lifted
      .read[E]
  }*/



}
