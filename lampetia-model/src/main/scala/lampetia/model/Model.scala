package lampetia.model

import scala.language.implicitConversions

/**
 * @author Hossam Karim
 */

trait Feature

trait Property[+A] {
  def name: String
  def set(feature: Feature): Property[A]
  def features: Seq[Feature]
}

trait Composite[+A] {
  def properties: Seq[Property[_]]
  def features: Seq[Feature] = Seq.empty[Feature]
}

trait RefModel[+R] extends Composite[R]

trait DataModel[D] extends Composite[D]

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

trait Model[E] extends HasProperties {
  def name: String
  def properties: Properties = Seq.empty[Property[_]]
  def features: Seq[Feature] = Seq.empty[Feature]
}




