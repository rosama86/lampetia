package lampetia.model

import org.joda.time.DateTime

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

case class SubjectId(id: String) extends AnyVal
case class TimeSignature(createdAt: DateTime, updatedAt: DateTime)
case class SubjectSignature(createdBy: SubjectId, updatedBy: SubjectId)

trait Property[+A] {
  def sqlName: String
  def typeCastInInsert: Option[String]
}

case class SimpleProperty[A](sqlName: String, typeCastInInsert: Option[String]) extends Property[A]

trait Composite[+A] {
  def properties: Seq[Property[_]]
}

trait JsonProperty[+A] extends Property[A] with Composite[A]


trait TimestampComposite extends Composite[TimeSignature] {

  lazy val createdAt: Property[DateTime] = SimpleProperty[DateTime]("created_at", None)

  lazy val updatedAt: Property[DateTime] = SimpleProperty[DateTime]("updated_at", None)

  lazy val properties = Seq(createdAt, updatedAt)
}
object TimestampComposite extends TimestampComposite

trait SignatureComposite extends Composite[SubjectSignature] {
  lazy val createdBy: Property[SubjectId] = SimpleProperty[SubjectId]("created_by", None)
  lazy val updatedBy: Property[SubjectId] = SimpleProperty[SubjectId]("updated_by", None)
  lazy val properties = Seq(createdBy, updatedBy)
}
object SignatureComposite extends SignatureComposite


trait RefModel[+R] extends Composite[R]

case object NoRef extends RefModel[Nothing] {
  def properties: Seq[Property[_]] = Seq.empty[Property[_]]
}


trait DataModel[D] extends Composite[D]

case object NoData extends DataModel[Nothing] {
  def properties: Seq[Property[_]] = Seq.empty[Property[_]]
}

trait HasProperties {
  final type Properties = Seq[Property[_]]
  def properties: Properties
}

trait HasId[E] extends HasProperties { this: Model[E] =>
  type Id
  def id: Property[Id]
  abstract override def properties: Properties = super.properties :+ id
}
trait HasRef[E] extends HasProperties { this: Model[E] =>
  type Ref
  def ref: RefModel[Ref]
  abstract override def properties: Properties = super.properties ++ ref.properties
}

trait HasData[E] extends HasProperties { this: Model[E] =>
  type Data
  def data: DataModel[Data]
  abstract override def properties: Properties = super.properties ++ data.properties
}

trait HasTimestamp[E] extends HasProperties { this: Model[E] =>
  def timestamp: TimestampComposite = TimestampComposite
  abstract override def properties: Properties = super.properties ++ timestamp.properties
}

trait HasSignature[E] extends HasProperties { this: Model[E] =>
  def signature: SignatureComposite = SignatureComposite
  abstract override def properties: Properties = super.properties ++ signature.properties
}

trait Model[E] extends HasProperties {
  def sqlName: String

  // factory method for  creating Properties
  def property[A](name: String, typeCastInInsert: Option[String] = None): Property[A] =
    SimpleProperty[A](name, typeCastInInsert)

  def properties: Properties = Seq.empty[Property[_]]

}




