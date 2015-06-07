package lampetia.sql.dsl.dialect

import lampetia.model.Property
import lampetia.sql.ast._
import lampetia.sql.syntax.{ConnectionSourceFactories, JdbcCodec}

/**
 * @author Hossam Karim
 */

package object postgres extends PgDsl with JdbcCodec with ConnectionSourceFactories {

  implicit class Strings(val value: String) extends AnyVal with StringsDsl

  implicit class Integers(val value: Int) extends AnyVal with IntegersDsl

  implicit class OperandOpsEx[A <: Operand](val value: A) extends AnyVal with OperandOps

  implicit class OperatorOpsEx[A <: Operator](val value: A) extends AnyVal with OperatorOps

  implicit class Symbols(val symbol: Symbol) extends AnyVal with SymbolsDsl

  implicit class PropertyLifter[A](val property: Property[A]) extends AnyVal with PropertyLifterDsl[A]

  implicit class StringSqlImplicits(val sqlString: String) extends AnyVal with StringsSql

  implicit class SqlIOOpsImplicits[A](val sqlIO: SqlIO[A]) extends AnyVal with SqlIOOps[A]

  implicit class LiftParameterImplicits[A](val instance: A) extends AnyVal with LiftParameter[A]

  implicit class LiftAstNodeImplicits(val node: Operand) extends AnyVal with LiftAstNode

  implicit class LiftSqlStringImplicits(val context: StringContext) extends AnyVal with LiftSqlString

  implicit class LiftSqlIO[A](val io: SqlIO[A]) extends AnyVal with LiftIO[A]

}
