package lampetia.sql

import lampetia.model.Model
import lampetia.model.sql._
import lampetia.sql.ast.{Operand, Operator}
import lampetia.sql.dsl._


trait Operations {

  trait ModelSchema[E] extends Any {
    def model: Model[E]
    def schemaPrefixed: Operand = model.sqlSchema match {
      case Some(v) => v.identifier dot model
      case None    => model
    }
  }

  trait Find[E] extends Any { ms: ModelSchema[E] =>

    def find(implicit ce: Consume[E]): SqlIO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).lifted.readSqlIO[E]

    def find[F <: Operator](filter: F)(implicit ce: Consume[E]): SqlIO[Seq[E]] =
      select(model.properties:_*).from(ms.schemaPrefixed).where(filter).lifted.readSqlIO[E]

    def findOne[F <: Operator](filter: F)(implicit ce: Consume[E]): SqlIO[Option[E]] =
      find(filter).map(_.headOption)

  }

  trait Delete[E] extends Any { ms: ModelSchema[E] =>

    def delete: SqlIO[Int] =
      deleteFrom(ms.schemaPrefixed).lifted.writeSqlIO

    def delete[F <: Operator](filter: F): SqlIO[Int] =
      deleteFrom(ms.schemaPrefixed).where(filter).lifted.writeSqlIO
  }

  trait Update[E] extends Any { ms: ModelSchema[E] =>

    def update[F <: Operator](first: (Operand, Operand), next: (Operand, Operand)*)(filter: F): SqlIO[Int] = {
      val init = Q.update(ms.schemaPrefixed).set(first._1, first._2)
      next.foldLeft(init)( (upd, c) => upd.set(c._1, c._2)).where(filter).lifted.writeSqlIO
    }

  }

  trait DDL[E] extends Any { ms: ModelSchema[E] =>

    def create(implicit dst: DefaultSqlType): SqlIO[Int] = createTable(model).lifted.writeSqlIO

  }

}