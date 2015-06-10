package lampetia.model

import lampetia.model.util._

/**
 * @author Hossam Karim
 */

package object sql {

  sealed trait SqlFeature extends Any with Feature

  case object Optional extends SqlFeature
  def optional = Optional

  case class SqlName(value: String) extends AnyVal with SqlFeature

  def name(value: String): SqlFeature = SqlName(value)

  case class SqlType(value: String) extends AnyVal with SqlFeature

  def `type`(value: String): SqlFeature = SqlType(value)

  case class SqlSchema(value: String) extends AnyVal with SqlFeature

  def schema(value: String): SqlSchema = SqlSchema(value)

  case class SqlPrimaryKey(name: Option[String], properties: Seq[Property[_, _]]) extends SqlFeature

  def primaryKey(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlPrimaryKey =
    SqlPrimaryKey(Some(name), property +: properties)

  def primaryKey(property: Property[_, _], properties: Property[_, _]*): SqlPrimaryKey =
    SqlPrimaryKey(None, property +: properties)

  case class SqlForeignKey(name: Option[String], keys: Seq[Property[_, _]], references: Seq[Property[_, _]]) extends SqlFeature

  def foreignKey(name: String)(key: Property[_, _], keys: Property[_, _]*)(ref: Property[_, _], references: Property[_, _]*): SqlForeignKey =
    SqlForeignKey(Some(name), key +: keys, ref +: references)

  def foreignKey(key: Property[_, _], keys: Property[_, _]*)(ref: Property[_, _], references: Property[_, _]*): SqlForeignKey =
    SqlForeignKey(None, key +: keys, ref +: references)

  case class SqlIndex(name: Option[String], properties: Seq[Property[_, _]], unique: Boolean) extends SqlFeature

  def index(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(Some(name), property +: properties, unique = false)
  def index(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(None, property +: properties, unique = false)
  def uniqueIndex(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(Some(name), property +: properties, unique = true)
  def uniqueIndex(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(None, property +: properties, unique = true)


  implicit class ModelFeatures[A](val model: Model[A]) extends AnyVal {

    def sqlName: String = model.features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(model.name.snakeCase)

    def sqlSchema: Option[String] = model.features.collectFirst {
      case SqlSchema(value) => value
    }

    def sqlPrimaryKey: Option[SqlPrimaryKey] = model.features.collectFirst {
      case pk: SqlPrimaryKey => Some(pk)
    }.getOrElse(None)

    def sqlForeignKeys: Seq[SqlForeignKey] = model.features.collect {
      case fk: SqlForeignKey => fk
    }

    def sqlIndexes: Seq[SqlIndex] = model.features.collect {
      case i: SqlIndex => i
    }
  }

  implicit class PropertyFeatures[E, A](val p: Property[E, A]) {

    def sqlName: String = p.features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(p.name.snakeCase)

    def sqlType(implicit dst: DefaultSqlType): String = p.features.collectFirst {
      case SqlType(value) => value
    }.getOrElse(dst.name)

    def optional: Boolean = p.features.collectFirst {
      case Optional => true
    }.getOrElse(false)

  }

}
