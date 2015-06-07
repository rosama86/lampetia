package lampetia.sql.syntax

import lampetia.codec.Codec
import lampetia.io.BackendIO
import lampetia.sql.ast.{Operand, Operator}
import org.joda.time.DateTime
import org.slf4j.LoggerFactory

import scala.language.{higherKinds, implicitConversions}

/**
 * @author Hossam Karim
 */

trait SqlCodec extends Codec with BackendIO {

  private val log = LoggerFactory.getLogger("sql-codec")

  // The connection type of the underlying driver
  type Connection

  trait ConnectionSource {
    def connection: Connection
    def done(connection: Connection)
    def shutdown(): Unit
  }

  type Context = ConnectionSource

  //def connectionSource: ConnectionSource

  trait SqlReader {
    def readString: String
    def readBoolean: Boolean
    def readInt: Int
    def readLong: Long
    def readDate: DateTime
    def readTimestamp: DateTime
  }

  trait SqlWriter {
    def writeNull(sqlType: Int): SqlWriter
    def writeString(value: String): SqlWriter
    def writeBoolean(value: Boolean): SqlWriter
    def writeInt(value: Int): SqlWriter
    def writeLong(value: Long): SqlWriter
    def writeDate(value: DateTime): SqlWriter
    def writeTimestamp(value: DateTime): SqlWriter
  }

  type Reader = SqlReader
  type Writer = SqlWriter

  trait Parameter {
    type A
    def name: String = "?"
    def value: A
    def producer: Produce[A]
  }

  object Parameter {
    def apply[T](parameterValue: T, parameterProducer: Produce[T]): Parameter = new Parameter {
      type A = T
      val value: A = parameterValue
      val producer: Produce[A] = parameterProducer
    }
    def apply[T](paramterName: String, parameterValue: T, parameterProducer: Produce[T]): Parameter = new Parameter {
      type A = T
      override val name: String = paramterName
      val value: A = parameterValue
      val producer: Produce[A] = parameterProducer
    }
  }

  type SqlIO[A] = IO[A]

  sealed trait Sql {
    def sqlString: String
    def readSqlIO[R](implicit consumer: Consume[R]): SqlIO[Seq[R]]
    def writeSqlIO: SqlIO[Int]
  }

  sealed trait InterpretedSql extends Sql {
    def set[A](parameterValue: A)(implicit producer: Produce[A]): PositionalParametersSql
    def set[A](parameterName: String, parameterValue: A)(implicit producer: Produce[A]): NamedParametersSql
  }

  sealed trait PositionalParametersSql extends Sql {
    def set[A](parameterValue: A)(implicit producer: Produce[A]): PositionalParametersSql
  }

  sealed trait NamedParametersSql extends Sql {
    def set[A](parameterName: String, parameterValue: A)(implicit producer: Produce[A]): NamedParametersSql
    def positional: PositionalParametersSql
  }

  case class PlainSql(sqlString: String) extends InterpretedSql {
    def set[A](parameterValue: A)(implicit producer: Produce[A]): PositionalParametersSql =
      ParameterizedSql(sqlString, Seq(Parameter(parameterValue, producer)))

    def set[A](parameterName: String, parameterValue: A)(implicit producer: Produce[A]): NamedParametersSql =
      NamedParameterizedSql(sqlString, Seq(Parameter(parameterName, parameterValue, producer)))

    def readSqlIO[R](implicit consumer: Consume[R]): ReadPlainSql[R] =
      createReadPlainSql(this, consumer)

    def writeSqlIO: WritePlainSql = createWritePlainSql(this)
  }

  case class ParameterizedSql(sqlString: String, parameters: Seq[Parameter]) extends PositionalParametersSql {
    def set[A](parameterValue: A)(implicit producer: Produce[A]): PositionalParametersSql =
      copy(parameters = parameters :+ Parameter(parameterValue, producer))

    def readSqlIO[R](implicit consumer: Consume[R]): ReadParameterizedSql[R] =
      createReadParameterizedSql(this, consumer)

    def writeSqlIO: WriteParameterizedSql = createWriteParameterizedSql(this)
  }

  case class ReOrdering(sql: String, original: Seq[Parameter], reordered: Seq[Parameter])

  def namedToPositional(named: NamedParameterizedSql): ParameterizedSql = {
    val query = named.sqlString
    val r = "#\\{(\\w+)\\}".r

    val stringParameters = r.findAllIn(query)

    val reordering: ReOrdering = ReOrdering(query, named.parameters, Seq.empty[Parameter])

    val reordered = stringParameters.foldLeft(reordering) { (acc, current) =>
      acc.original.find(x => s"#{${x.name}}" == current) match {
        case Some(param) => acc.copy(sql = r.replaceFirstIn(acc.sql, "?"), reordered = acc.reordered :+ param)
        case None        =>
          log.warn(s"Parameter $current is not set, ingoring")
          acc
      }
    }

    ParameterizedSql(reordered.sql, reordered.reordered)

  }

  case class NamedParameterizedSql(sqlString: String, parameters: Seq[Parameter]) extends NamedParametersSql {

    def set[A](parameterName: String, parameterValue: A)(implicit producer: Produce[A]): NamedParametersSql =
      copy(parameters = parameters :+ Parameter(parameterName, parameterValue, producer))

    def positional: PositionalParametersSql = namedToPositional(this)

    def readSqlIO[R](implicit consumer: Consume[R]): ReadParameterizedSql[R] =
      createReadParameterizedSql(namedToPositional(this), consumer)

    def writeSqlIO: WriteParameterizedSql =
      createWriteParameterizedSql(namedToPositional(this))
  }

  trait ReadPlainSql[R] extends SqlIO[Seq[R]] {
    def plainSql: PlainSql
    def consumer: Consume[R]
  }

  def createReadPlainSql[R](plainSql: PlainSql, consumer: Consume[R]): ReadPlainSql[R]

  trait WritePlainSql extends SqlIO[Int] {
    def plainSql: PlainSql
  }

  def createWritePlainSql(plainSql: PlainSql): WritePlainSql

  trait ReadParameterizedSql[R] extends SqlIO[Seq[R]] {
    def parameterizedSql: ParameterizedSql
    def consumer: Consume[R]
  }

  def createReadParameterizedSql[R](parameterizedSql: ParameterizedSql, consumer: Consume[R]): ReadParameterizedSql[R]

  trait WriteParameterizedSql extends SqlIO[Int] {
    def parameterizedSql: ParameterizedSql
  }

  def createWriteParameterizedSql[R](parameterizedSql: ParameterizedSql): WriteParameterizedSql


  trait TransactionalSqlIO[R] extends SqlIO[R] {
    def sqlIO: SqlIO[R]
  }

  def createTransactionalSqlIO[R](sqlIO: SqlIO[R]): TransactionalSqlIO[R]


  trait StringsSql extends Any {
    def sqlString: String
    def sql: PlainSql = PlainSql(sqlString)
  }

  trait SqlIOOps[A] extends Any {
    def sqlIO: SqlIO[A]
    def transactionally: TransactionalSqlIO[A] = createTransactionalSqlIO(sqlIO)
  }

  case class PresetParameterNode(parameter: Parameter) extends Operand {
    val sqlString: String = "?"
  }

  trait LiftParameter[A] extends Any {
    def instance: A
    def bind(implicit p: Produce[A]): PresetParameterNode = PresetParameterNode(Parameter(instance, p))
  }

  trait LiftAstNode extends Any {
    def node: Operand
    private def ps(op: Operand): Seq[Parameter] = op match {
      case x: Operator            =>
        x.operands.flatMap(ps)
      case x: PresetParameterNode =>
        Seq(x.parameter)
      case x                      =>
        Seq()
    }

    def parameters: Seq[Parameter] = ps(node)

    def sql: InterpretedSql = PlainSql(node.sqlString)

    // return Sql to prevent setting other parameters
    def lifted: Sql = ParameterizedSql(node.sqlString, parameters)

    def liftedDebug: Sql = {
      val result = ParameterizedSql(node.sqlString, parameters)
      log.info("ParameterizedSql: START")
      log.info("SQL: START")
      log.info(result.sqlString)
      log.info("SQL: END")
      log.info("Parameters: START")
      result.parameters.foreach { p =>
        log.info(s"${p.name} -> ${p.value}")
      }
      log.info("Parameters: END")
      log.info("ParameterizedSql: END")
      result
    }
  }

  trait LiftSqlString extends Any {
    def context: StringContext
    private def p[A: Produce](a: A): Parameter = Parameter(a, implicitly[Produce[A]])

    def sql(): Sql = ParameterizedSql(context.parts.mkString, Seq.empty[Parameter])
    def sql[A1: Produce](a1: A1): Sql =
      ParameterizedSql(context.parts.mkString("?"), Seq(p(a1)))
    def sql[A1: Produce, A2: Produce](a1: A1, a2: A2): Sql =
      ParameterizedSql(context.parts.mkString("?"), Seq(p(a1), p(a2)))
  }




  implicit val consumeString: Consume[String] = _.readString
  implicit val produceString: Produce[String] = a => p => p.writeString(a)

  implicit val consumeBoolean: Consume[Boolean] = _.readBoolean
  implicit val produceBoolean: Produce[Boolean] = a => p => p.writeBoolean(a)

  implicit val consumeInt: Consume[Int] = _.readInt
  implicit val produceInt: Produce[Int] = a => p => p.writeInt(a)

  implicit val consumeLong: Consume[Long] = _.readLong
  implicit val produceLong: Produce[Long] = a => p => p.writeLong(a)



}
