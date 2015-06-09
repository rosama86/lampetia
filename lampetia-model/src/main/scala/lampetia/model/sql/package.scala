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

  case class SqlPrimaryKey(name: String, properties: Seq[Property[_, _]]) extends SqlFeature

  def primaryKey(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlPrimaryKey =
    SqlPrimaryKey(name, property +: properties)

  case class SqlForeignKey(name: String, keys: Seq[Property[_, _]], references: Seq[Property[_, _]]) extends SqlFeature

  def foreignKey(name: String)(key: Property[_, _], keys: Property[_, _]*)(ref: Property[_, _], references: Property[_, _]*): SqlForeignKey =
    SqlForeignKey(name, key +: keys, ref +: references)

  case class SqlIndex(name: String, properties: Seq[Property[_, _]], unique: Boolean) extends SqlFeature

  def index(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(name, property +: properties, unique = false)
  def uniqueIndex(name: String)(property: Property[_, _], properties: Property[_, _]*): SqlIndex =
    SqlIndex(name, property +: properties, unique = true)


  implicit class ModelFeatures[A](val model: Model[A]) extends AnyVal {

    def sqlName: String = model.features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(model.name.snakeCase)

    def sqlSchema: Option[String] = model.features.collectFirst {
      case SqlSchema(value) => value
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
