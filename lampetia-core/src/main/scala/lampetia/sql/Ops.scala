package lampetia.sql

import lampetia.io.BackendIO
import lampetia.model._
import lampetia.model.sql._
import lampetia.sql.ast.{QueryNodeBuilder, Dsl, Operand, Operator}
import lampetia.sql.dialect.Dialect

/**
 * @author Hossam Karim
 */
trait Ops { self: Dsl with Dialect with SqlCodec with JdbcCodec with BackendIO =>

  implicit def defaultSqlType: DefaultSqlType

  implicit class Strings(val value: String) extends StringsDsl

  implicit class Integers(val value: Int) extends IntegersDsl

  implicit class OperandOpsEx[A <: Operand](val value: A) extends OperandOps

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends OperatorOps

  implicit class Symbols(val symbol: Symbol) extends SymbolsDsl {
    def value: Operand = asIdentifier
  }

  implicit class UpdateCouple[A](val property: Property[A]) extends UpdateCoupleDsl[A]

  implicit class PropertyLifter[A](val property: Property[A]) extends PropertyLifterDsl[A] {
    def value: Operand = asColumnIdentifier
  }

  implicit class StringSqlImplicits(val sqlString: String) extends StringsSql

  implicit class SqlIOOpsImplicits[A](val sqlIO: IO[A]) extends IOOps[A]

  implicit class LiftParameterImplicits[A](val instance: A) extends LiftParameter[A]

  implicit class LiftAstNodeImplicits(val node: Operand) extends LiftAstNode

  implicit class LiftSqlStringImplicits(val context: StringContext) extends LiftSqlString

  implicit class LiftSqlIO[A](val io: IO[A]) extends LiftIO[A]

  def positionalParameter(p: Property[_]) = p.sqlCast match {
    case Some(tn) => ?.cast(tn.typeName)
    case None     => ?
  }

  trait ModelSchema[E] extends Any {
    def model: Model[E]
    def schemaPrefixed: Operand = model.sqlSchema match {
      case Some(v) => v.identifier dot model
      case None    => model
    }
  }

  trait Find[E] extends Any { ms: ModelSchema[E] =>

    def find(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).lifted.read[E]

    def find[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).where(filter).lifted.read[E]

    def findOne[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Option[E]] =
      find(filter).map(_.headOption)

  }

  trait Delete[E] extends Any { ms: ModelSchema[E] =>

    def delete: IO[Int] =
      deleteFrom(ms.schemaPrefixed).lifted.write

    def delete[F <: Operator](filter: F): IO[Int] =
      deleteFrom(ms.schemaPrefixed).where(filter).lifted.write
  }

  trait Update[E] extends Any { ms: ModelSchema[E] =>

    def update[F <: Operator](first: (Operand, Operand), next: (Operand, Operand)*)(filter: F): IO[Int] = {
      val init = Q.update(ms.schemaPrefixed).set(first._1, first._2)
      next.foldLeft(init)( (upd, c) => upd.set(c._1, c._2)).where(filter).lifted.write
    }

  }

  trait DDL[E] extends Any { ms: ModelSchema[E] =>

    def create: IO[Int] = {
      val c = createTable(model).sql.write
      val pk =
        model.sqlPrimaryKey.map(pk => primaryKey(model, pk).sql.write)
          .fold(pureIO(0))(identity)
      val fks =
        model.sqlForeignKeys.map(fk => foreignKey(model, fk).sql.write)
          .foldLeft(pureIO(0))( (l,r) => l.flatMap(_ => r))
      val idxs =
        model.sqlIndexes.map(idx => index(model, idx).sql.write)
          .foldLeft(pureIO(0))( (l,r) => l.flatMap(_ => r))

      c.flatMap(_ => pk).flatMap(_ => fks).flatMap(_ => idxs)
    }

    def createSql: Seq[Sql] = {
      val c = createTable(model).sql
      val pk =
        model.sqlPrimaryKey.map(pk => primaryKey(model, pk).sql)
      val fks =
        model.sqlForeignKeys.map(fk => foreignKey(model, fk).sql)
      val idxs =
        model.sqlIndexes.map(idx => index(model, idx).sql)
      pk match {
        case Some(v) =>
          c +: v +: (fks ++ idxs)
        case None =>
          c +: (fks ++ idxs)
      }
    }

  }

  implicit class Model0Ops[E](val model: Model[E])
    extends ModelSchema[E] with Find[E] with Update[E] with Delete[E] with DDL[E]

  implicit class Model01Ops[E, R](val model: Model[E] with HasRef[E, R] with CanCombine1[E, R])
    extends ModelSchema[E] with Find[E] with Update[E] with Delete[E] with DDL[E] {
    def insert(ref: R)(implicit pref: Produce[R]): IO[E] = {
      val ps = model.ref.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(ref).write.flatMap {
        case i if i > 0 => pureIO(model.combine(ref))
        case _          => failedIO[E](new Exception("No Instance"))
      }
    }
  }

  implicit class Model02Ops[E, D](val model: Model[E] with HasData[E, D] with CanCombine1[E, D])
    extends ModelSchema[E] with Find[E] with Update[E] with Delete[E] with DDL[E] {
    def insert(data: D)(implicit pref: Produce[D]): IO[E] = {
      val ps = model.data.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(data).write.flatMap {
        case i if i > 0 => pureIO(model.combine(data))
        case _          => failedIO[E](new Exception("No Instance"))
      }
    }
  }

  implicit class Model1Ops[E, Id](val model: Model[E] with HasId[E, Id] with CanCombine1[E, Id])
    extends ModelSchema[E] with Find[E] with Update[E] with Delete[E] {

    def insert(id: Id)(implicit pid: Produce[Id]): IO[E] =
      insertInto(schemaPrefixed).values(id.bind).lifted.write.flatMap {
        case i if i > 0 => pureIO(model.combine(id))
        case _          => failedIO[E](new Exception("No Instance"))
      }

    def insert(implicit pid: Produce[Id]): IO[E] =
      insert(model.generate)

  }

  implicit class Model2ROps[E, Id, R](val model: Model[E] with HasId[E, Id] with HasRef[E, R] with CanCombine2[E, Id, R])
    extends ModelSchema[E] with Find[E] with Delete[E] with Update[E] {

    def insert(id: Id, ref: R)(implicit pid: Produce[Id], pref: Produce[R]): IO[E] = {
      val ps = model.id +: model.ref.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).write.flatMap {
        case i if i > 0 => pureIO(model.combine(id, ref))
        case _          => failedIO[E](new Exception("No Instance"))
      }
    }

    def insert(ref: R)(implicit pid: Produce[Id], pref: Produce[R]): IO[E] =
      insert(model.generate, ref)

  }

  implicit class Model2DOps[E, Id, D](val model: Model[E] with HasId[E, Id] with HasData[E, D] with CanCombine2[E, Id, D])
    extends ModelSchema[E] with Find[E] with Delete[E] with Update[E] {

    def insert(id: Id, data: D)(implicit pid: Produce[Id], pdata: Produce[D]): IO[E] = {
      val ps = model.id +: model.data.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(data).write.flatMap {
        case i if i > 0 => pureIO(model.combine(id, data))
        case _          => failedIO[E](new Exception("No Instance"))
      }
    }

    def insert(data: D)(implicit pid: Produce[Id], pdata: Produce[D]): IO[E] =
      insert(model.generate, data)
  }

  implicit class Model3Ops[E, Id, D, R]
  (val model: Model[E] with HasId[E, Id] with HasRef[E, R] with HasData[E, D] with CanCombine3[E, Id, R, D])
    extends ModelSchema[E] with Find[E] with Delete[E] with Update[E] {

    def insert(id: Id, ref: R, data: D)(implicit pid: Produce[Id], pref: Produce[R], pdata: Produce[D]): IO[E] = {
      val ps = model.id +: (model.ref.properties ++ model.data.properties)
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(id).set(ref).set(data).write.flatMap {
        case i if i > 0 => pureIO(model.combine(id, ref, data))
        case _          => failedIO[E](new Exception("No Instance"))
      }
    }

    def insert(ref: R, data: D)(implicit pid: Produce[Id], pref: Produce[R], pdata: Produce[D]): IO[E] =
      insert(model.generate, ref, data)
  }

}
