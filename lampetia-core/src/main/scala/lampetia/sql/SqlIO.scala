package lampetia.sql

import lampetia.io.BackendIO
import lampetia.model.Property
import lampetia.sql.ast._
import org.slf4j.LoggerFactory

/**
 * @author Hossam Karim
 */

trait SqlIO extends BackendIO { codec: SqlCodec =>

  private val log = LoggerFactory.getLogger("sql-io")

  // The connection type of the underlying driver
  type Connection

  trait ConnectionSource {
    def connection: Connection
    def done(connection: Connection)
    def shutdown(): Unit
  }

  type Context = ConnectionSource

  trait Parameter[+A] {
    type T <: A
    def name: String = "?"
    def value: T
    def producer: Produce[T]
  }

  object Parameter {
    def apply[A](parameterValue: A, parameterProducer: Produce[A]): Parameter[A] = new Parameter[A] {
      type T = A
      val value: T = parameterValue
      val producer: Produce[T] = parameterProducer
    }
    def apply[A](paramterName: String, parameterValue: A, parameterProducer: Produce[A]): Parameter[A] = new Parameter[A] {
      type T = A
      override val name: String = paramterName
      val value: T = parameterValue
      val producer: Produce[T] = parameterProducer
    }
  }

  case class PresetParameterNode[+A](parameter: Parameter[A]) extends TypedParameterNode[A] {
    val sqlString: String = "?"
    def map[B](f: A => B)(implicit p: Produce[B]): PresetParameterNode[B] = PresetParameterNode[B](Parameter(f(value), p))
  }

  sealed trait Sql {
    def sqlString: String
    def read[R](implicit consumer: Consume[R]): IO[Seq[R]]
    def write: IO[Int]
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

    def read[R](implicit consumer: Consume[R]): ReadPlainSql[R] =
      createReadPlainSql(this, consumer)

    def write: WritePlainSql = createWritePlainSql(this)
  }

  case class ParameterizedSql(sqlString: String, parameters: Seq[Parameter[_]]) extends PositionalParametersSql {
    def set[A](parameterValue: A)(implicit producer: Produce[A]): PositionalParametersSql =
      copy(parameters = parameters :+ Parameter(parameterValue, producer))

    def read[R](implicit consumer: Consume[R]): ReadParameterizedSql[R] =
      createReadParameterizedSql(this, consumer)

    def write: WriteParameterizedSql = createWriteParameterizedSql(this)
  }

  case class ReOrdering(sql: String, original: Seq[Parameter[_]], reordered: Seq[Parameter[_]])

  def namedToPositional(named: NamedParameterizedSql): ParameterizedSql = {
    val query = named.sqlString
    val r = "#\\{(\\w+)\\}".r

    val stringParameters = r.findAllIn(query)

    val reordering: ReOrdering = ReOrdering(query, named.parameters, Seq.empty[Parameter[_]])

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

  case class NamedParameterizedSql(sqlString: String, parameters: Seq[Parameter[_]]) extends NamedParametersSql {

    def set[A](parameterName: String, parameterValue: A)(implicit producer: Produce[A]): NamedParametersSql =
      copy(parameters = parameters :+ Parameter(parameterName, parameterValue, producer))

    def positional: PositionalParametersSql = namedToPositional(this)

    def read[R](implicit consumer: Consume[R]): ReadParameterizedSql[R] =
      createReadParameterizedSql(namedToPositional(this), consumer)

    def write: WriteParameterizedSql =
      createWriteParameterizedSql(namedToPositional(this))
  }

  trait ReadPlainSql[R] extends IO[Seq[R]] {
    def plainSql: PlainSql
    def consumer: Consume[R]
  }

  def createReadPlainSql[R](plainSql: PlainSql, consumer: Consume[R]): ReadPlainSql[R]

  trait WritePlainSql extends IO[Int] {
    def plainSql: PlainSql
  }

  def createWritePlainSql(plainSql: PlainSql): WritePlainSql

  trait ReadParameterizedSql[R] extends IO[Seq[R]] {
    def parameterizedSql: ParameterizedSql
    def consumer: Consume[R]
  }

  def createReadParameterizedSql[R](parameterizedSql: ParameterizedSql, consumer: Consume[R]): ReadParameterizedSql[R]

  trait WriteParameterizedSql extends IO[Int] {
    def parameterizedSql: ParameterizedSql
  }

  def createWriteParameterizedSql[R](parameterizedSql: ParameterizedSql): WriteParameterizedSql


  trait TransactionalIO[R] extends IO[R] {
    def sqlIO: IO[R]
  }

  def createTransactionalIO[R](sqlIO: IO[R]): TransactionalIO[R]


  trait StringsSql extends Any {
    def sqlString: String
    def sql: PlainSql = PlainSql(sqlString)
  }

  trait IOOps[A] extends Any {
    def sqlIO: IO[A]
    def transactionally: TransactionalIO[A] = createTransactionalIO(sqlIO)
  }

  trait TypedParameterNode[+A] extends TypedOperand[A] {
    def parameter: Parameter[A]
    def value: A = parameter.value
    def cast(other: TypeNode)(implicit b: CastNodeBuilder): TypedParameterNode[A] =
      TypedParameterNodeAdapter[A](this, b(this, other))
  }

  case class TypedParameterNodeAdapter[A](tpn: TypedParameterNode[A], adaptee: Operand) extends TypedParameterNode[A] {
    def parameter: Parameter[A] = tpn.parameter
    def sqlString: String = adaptee.sqlString
  }


  trait LiftParameter[A] extends Any {
    def instance: A
    def bind(implicit p: Produce[A]): PresetParameterNode[A] = PresetParameterNode[A](Parameter(instance, p))
  }

  trait LiftAstNode extends Any {
    def node: Operand
    private def ps(op: Operand): Seq[Parameter[_]] = op match {
      case x: Operator            =>
        x.operands.flatMap(ps)
      case x: TypedParameterNode[_] =>
        Seq(x.parameter)
      case x                      =>
        Seq()
    }

    def parameters: Seq[Parameter[_]] = ps(node)

    def sql: InterpretedSql = {
    if (log.isDebugEnabled) {
      log.debug("Plain SQL")
      log.debug(node.sqlString)
      log.debug("-----------------------------")
    }
      PlainSql(node.sqlString)
    }

    // return Sql to prevent setting other parameters
    def lifted: Sql = {
      val result = ParameterizedSql(node.sqlString, parameters)
      if (log.isDebugEnabled) {
        log.debug("Lifted SQL")
        log.debug(result.sqlString)
        if (result.parameters.nonEmpty)
          log.debug("Parameters: ")
        else
          log.debug("No Parameters")
        result.parameters.foreach { p =>
          log.debug(s"${p.name} -> ${p.value}")
        }
        log.debug("-----------------------------")
      }
      result
    }
  }

  trait LiftSqlString extends Any {
    def context: StringContext
    private def p[A: Produce](a: A): Parameter[A] = Parameter(a, implicitly[Produce[A]])

    def sql(): Sql = ParameterizedSql(context.parts.mkString, Seq.empty[Parameter[_]])
    def sql[A1: Produce](a1: A1): Sql =
      ParameterizedSql(context.parts.mkString("?"), Seq(p(a1)))
    def sql[A1: Produce, A2: Produce](a1: A1, a2: A2): Sql =
      ParameterizedSql(context.parts.mkString("?"), Seq(p(a1), p(a2)))
  }

  case class Couple[A](column: ColumnIdentifierNode[A], operand: TypedParameterNode[A])


  trait CoupleDsl[A] extends Any {
    def property: Property[A]
    def lift(property: Property[A])(implicit b: ColumnIdentifierNodeBuilder): ColumnIdentifierNode[A] = b(property)
    def :=(bound: TypedParameterNode[A])(implicit b: ColumnIdentifierNodeBuilder): Couple[A] = Couple(lift(property), bound)
  }


}
