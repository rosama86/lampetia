package lampetia.sql

import lampetia.io.BackendIO
import lampetia.model._
import lampetia.model.sql._
import lampetia.sql.ast.{QueryNodeBuilder, Dsl, Operand, Operator}
import lampetia.sql.dialect.Dialect

/**
 * @author Hossam Karim
 */
trait Ops { self: Dsl with Dialect with SqlIO with SqlCodec with BackendIO =>

  implicit def sqlTypes: SqlTypes

  implicit class Strings(val value: String) extends StringsDsl

  implicit class Integers(val value: Int) extends IntegersDsl

  implicit class OperandOpsEx[A <: Operand](val value: A) extends OperandOps

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends OperatorOps

  implicit class Symbols(val symbol: Symbol) extends SymbolsDsl {
    def value: Operand = asIdentifier
  }

  implicit class CoupleEx[A](val property: Property[A]) extends CoupleDsl[A]

  implicit class PropertyLifter[A](val property: Property[A]) extends PropertyLifterDsl[A] {
    def value: Operand = asColumnIdentifier
  }

  implicit class StringSqlImplicits(val sqlString: String) extends StringsSql

  implicit class SqlIOOpsImplicits[A](val sqlIO: IO[A]) extends IOOps[A]

  implicit class LiftParameterImplicits[A](val instance: A) extends LiftParameter[A]

  implicit class LiftAstNodeImplicits(val node: Operand) extends LiftAstNode

  implicit class LiftSqlStringImplicits(val context: StringContext) extends LiftSqlString

  implicit class LiftSqlIO[A](val io: IO[A]) extends LiftIO[A]

  /*def positionalParameter(p: Property[_]) = p.sqlCast match {
    case Some(tn) => ?.cast(tn.typeName)
    case None     => ?
  }*/

  trait ModelSchema[E] extends Any {
    def model: Model[E]
    def schemaPrefixed: Operand = model.sqlQualifiedName.identifier
  }

  trait Find[E] extends Any { ms: ModelSchema[E] =>

    def find(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).lifted.read[E]

    def find[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).where(filter).lifted.read[E]

    def findOne[F <: Operator](filter: F)(implicit ce: Consume[E], b: QueryNodeBuilder): IO[Option[E]] =
      find(filter).map(_.headOption)

  }



  trait Insert[E] extends Any { ms: ModelSchema[E] =>

    def insert(couples: Couple[_]*): IO[Int] =
      insertInto(schemaPrefixed, couples.map(_.column.property):_*)
      .values(couples.map(_.operand):_*)
      .lifted
      .write


    /*def insert(instance: E)(implicit p: Produce[E]): IO[E] = {
      val ps = model.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(instance).write.flatMap {
        case i if i > 0 => IO.pure(instance)
        case _          => IO.failed[E](new Exception("No Instance"))
      }
    }

    def +=(instance: E)(implicit p: Produce[E]): IO[Int] = {
      val ps = model.properties
      val vs = ps.map(positionalParameter)
      insertInto(schemaPrefixed, ps:_*).values(vs:_*).sql.set(instance).write
    }

    def ++=(instances: E*)(implicit p: Produce[E]): IO[Int] = {
      val ios: Seq[IO[Int]] = instances.map(+=)
      IO.seq(ios).map(_.sum)
    }

    protected def insert(result: BuildResult[E])(implicit p: Produce[E]): IO[E] =
      result match {
        case BuildSuccess(instance) => insert(instance)
        case BuildFailure(e)        => IO.failed[E](e)
      }

    def insert(implicit cc: CanBuild0[E], p: Produce[E]): IO[E] =
      insert(cc.build)

    def insert[A1](a1: A1)
                  (implicit cc: CanBuild1[E, A1], p: Produce[E]): IO[E] =
      insert(cc.build(a1))

    def insert[A1, A2](a1: A1, a2: A2)
                      (implicit cc: CanBuild2[E, A1, A2], p: Produce[E]): IO[E] =
      insert(cc.build(a1, a2))

    def insert[A1, A2, A3](a1: A1, a2: A2, a3: A3)
                          (implicit cc: CanBuild3[E, A1, A2, A3], p: Produce[E]): IO[E] =
      insert(cc.build(a1, a2, a3))

    def insert[A1, A2, A3, A4](a1: A1, a2: A2, a3: A3, a4: A4)
                              (implicit cc: CanBuild4[E, A1, A2, A3, A4], p: Produce[E]): IO[E] =
      insert(cc.build(a1, a2, a3, a4))*/

  }

  trait Delete[E] extends Any { ms: ModelSchema[E] =>

    def delete: IO[Int] =
      deleteFrom(ms.schemaPrefixed).lifted.write

    def delete[F <: Operator](filter: F): IO[Int] =
      deleteFrom(ms.schemaPrefixed).where(filter).lifted.write
  }

  trait Update[E] extends Any { ms: ModelSchema[E] =>

    def update[F <: Operator](first: Couple[_], next: Couple[_]*)(filter: F): IO[Int] = {
      val init = Q.update(ms.schemaPrefixed).set(first.column, first.operand)
      next.foldLeft(init)( (upd, c) => upd.set(c.column, c.operand)).where(filter).lifted.write
    }

  }

  trait DDL[E] extends Any { ms: ModelSchema[E] =>

    def create: IO[Int] = {
      val c: IO[Int] = createTable(model).sql.write
      val pk: IO[Int] =
        model.sqlPrimaryKey.map(pk => primaryKey(model, pk).sql.write)
          .fold(IO.pure(0))(identity)
      val fks: Seq[IO[Int]] =
        model.sqlForeignKeys.map(fk => foreignKey(model, fk).sql.write)
          //.foldLeft(IO.pureIO(0))( (l,r) => l.flatMap(_ => r))
      val idxs: Seq[IO[Int]] =
        model.sqlIndexes.map(idx => index(model, idx).sql.write)
          //.foldLeft(IO.pureIO(0))( (l,r) => l.flatMap(_ => r))

      //c.flatMap(_ => pk).flatMap(_ => fks).flatMap(_ => idxs)
      IO.seq(Seq(c, pk) ++ fks ++ idxs).map(_.sum)
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


}
