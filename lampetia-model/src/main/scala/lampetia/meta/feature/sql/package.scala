package lampetia.meta.feature

import lampetia.meta.{Property, Feature, Model}
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

  case class SqlCast(typeName: String) extends AnyVal with SqlFeature

  def cast(typeName: String): SqlFeature= SqlCast(typeName)

  case class SqlSchema(value: String) extends AnyVal with SqlFeature

  def schema(value: String): SqlSchema = SqlSchema(value)

  case class SqlPrimaryKey(name: Option[String], properties: Seq[Property[_]]) extends SqlFeature

  def primaryKey(name: String)(property: Property[_], properties: Property[_]*): SqlPrimaryKey =
    SqlPrimaryKey(Some(name), property +: properties)

  def primaryKey(property: Property[_], properties: Property[_]*): SqlPrimaryKey =
    SqlPrimaryKey(None, property +: properties)

  case class SqlForeignKey[R](
    name: Option[String],
    keys: Seq[Property[_]],
    refModel: Model[R],
    references: Seq[Property[_]]) extends SqlFeature

  def foreignKey[R](name: String)(key: Property[_], keys: Property[_]*)
                   (refModel: Model[R], ref: Property[_], references: Property[_]*): SqlForeignKey[R] =
    SqlForeignKey[R](Some(name), key +: keys, refModel, ref +: references)

  def foreignKey[R](key: Property[_], keys: Property[_]*)
                   (refModel: Model[R], ref: Property[_], references: Property[_]*): SqlForeignKey[R] =
    SqlForeignKey[R](None, key +: keys, refModel, ref +: references)

  case class SqlIndex(name: Option[String], properties: Seq[Property[_]], unique: Boolean) extends SqlFeature

  def index(name: String)(property: Property[_], properties: Property[_]*): SqlIndex =
    SqlIndex(Some(name), property +: properties, unique = false)
  def index(property: Property[_], properties: Property[_]*): SqlIndex =
    SqlIndex(None, property +: properties, unique = false)
  def uniqueIndex(name: String)(property: Property[_], properties: Property[_]*): SqlIndex =
    SqlIndex(Some(name), property +: properties, unique = true)
  def uniqueIndex(property: Property[_], properties: Property[_]*): SqlIndex =
    SqlIndex(None, property +: properties, unique = true)


  implicit class ModelFeatures[A](val model: Model[A]) extends AnyVal {

    def features = model.features.reverse

    def sqlName: String = features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(model.modelName.snakeCase)

    def sqlSchema: Option[String] = features.collectFirst {
      case SqlSchema(value) => value
    }

    def sqlQualifiedName: String = sqlSchema match {
      case Some(schema) => s"$schema.$sqlName"
      case None         => sqlName
    }

    def sqlPrimaryKey: Option[SqlPrimaryKey] = features.collectFirst {
      case pk: SqlPrimaryKey => Some(pk)
    }.getOrElse(None)

    def sqlForeignKeys: Seq[SqlForeignKey[_]] = features.collect {
      case fk: SqlForeignKey[_] => fk
    }

    def sqlIndexes: Seq[SqlIndex] = features.collect {
      case i: SqlIndex => i
    }
  }

  implicit class PropertyFeatures[A](val p: Property[A]) {

    def features = p.features.reverse

    def sqlName: String = features.collectFirst {
      case SqlName(value) => value
    }.getOrElse(p.propertyName.snakeCase)

    def sqlType(implicit dst: SqlTypes): String = features.collectFirst {
      case SqlType(value) => value
    }.getOrElse(dst.name(p.propertyType))

    def optional: Boolean = features.collectFirst {
      case Optional => true
    }.getOrElse(false)

    def sqlCast: Option[String] = features.collectFirst {
      case SqlCast(v) => Some(v)
    }.getOrElse(None)

  }

}
