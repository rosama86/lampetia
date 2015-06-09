package lampetia.sql

import lampetia.model._
import lampetia.sql.ast._


/**
 * @author Hossam Karim
 */

package object dsl
  extends Dsl
  with    JdbcCodec
  with    ConnectionSourceFactories
  with    Operations {

  implicit class Strings(val value: String) extends AnyVal with StringsDsl

  implicit class Integers(val value: Int) extends AnyVal with IntegersDsl

  implicit class OperandOpsEx[A <: Operand](val value: A) extends AnyVal with OperandOps

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends AnyVal with OperatorOps

  implicit class Symbols(val symbol: Symbol) extends AnyVal with SymbolsDsl

  implicit class UpdateCouple[E, A](val property: Property[E, A]) extends AnyVal with UpdateCoupleDsl[E, A]

  implicit class PropertyLifter[E, A](val property: Property[E, A]) extends AnyVal with PropertyLifterDsl[E, A]

  implicit class StringSqlImplicits(val sqlString: String) extends AnyVal with StringsSql

  implicit class SqlIOOpsImplicits[A](val sqlIO: SqlIO[A]) extends AnyVal with SqlIOOps[A]

  implicit class LiftParameterImplicits[A](val instance: A) extends AnyVal with LiftParameter[A]

  implicit class LiftAstNodeImplicits(val node: Operand) extends AnyVal with LiftAstNode

  implicit class LiftSqlStringImplicits(val context: StringContext) extends AnyVal with LiftSqlString

  implicit class LiftSqlIO[A](val io: SqlIO[A]) extends AnyVal with LiftIO[A]


  implicit class Model0Ops[E](val model: Model[E])
    extends AnyVal with ModelSchema[E] with Find[E] with Update[E] with Delete[E]

  implicit class Model1Ops[E, Id](val model: Model[E] with HasId[E, Id] with CanCombine1[E, Id])
    extends AnyVal with ModelSchema[E] with Find[E] with Update[E] with Delete[E] {
    def insert(id: Id)(implicit pid: Produce[Id]): SqlIO[Int] =
      insertInto(schemaPrefixed).values(id.bind).lifted.writeSqlIO
  }

  implicit class Model2ROps[E, Id, R](val model: Model[E] with HasId[E, Id] with HasRef[E, R] with CanCombine2[E, Id, R])
    extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {
    def instance(id: Id, ref: R): E = model.combine(id, ref)
    def find(id: Id)(implicit pid: Produce[Id], ce: Consume[E]): SqlIO[Option[E]] =
      find(model.id === id.bind).map(_.headOption)

    def insert(id: Id, ref: R)(implicit pid: Produce[Id], pref: Produce[R]): SqlIO[Int] = {
      val ps = model.id +: model.ref.properties
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).writeSqlIO
    }
  }
  implicit class Model2DOps[E, Id, D](val model: Model[E] with HasId[E, Id] with HasData[E, D] with CanCombine2[E, Id, D])
    extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {

    def instance(id: Id, data: D): E = model.combine(id, data)

    def insert(id: Id, data: D)(implicit pid: Produce[Id], pdata: Produce[D]): SqlIO[Int] = {
      val ps = model.id +: model.data.properties
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(data).writeSqlIO
    }

    def insert(data: D)(implicit pid: Produce[Id], pdata: Produce[D]): SqlIO[E] = {
      val ps = model.id +: model.data.properties
      val vs = ps.map(_ => ?)
      val id = model.generate
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(data).writeSqlIO.flatMap {
        case i if i > 0 => IOPure(model.combine(id, data))
        case _          => IOFailed(new Exception("No Instance"))
      }
    }
  }

  implicit class Model3Ops[E, Id, D, R]
  (val model: Model[E] with HasId[E, Id] with HasRef[E, R] with HasData[E, D] with CanCombine3[E, Id, R, D])
      extends AnyVal with ModelSchema[E] with Find[E] with Delete[E] with Update[E] {
    def instance(id: Id, ref: R, data: D): E = model.combine(id, ref, data)
    def insert(id: Id, ref: R, data: D)(implicit pid: Produce[Id], pref: Produce[R], pdata: Produce[D]): SqlIO[Int] = {
      val ps = model.id +: (model.ref.properties ++ model.data.properties)
      val vs = ps.map(_ => ?)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).set(data).writeSqlIO
    }
  }

}
