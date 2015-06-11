package lampetia.model

import scala.util.Try

/**
 * @author Hossam Karim
 */

trait Feature extends Any

trait Property[V] {
  def name: String
  def set(feature: Feature): Property[V]
  def features: Seq[Feature]
}

trait HasProperties[A] {
  def properties: Seq[Property[_]]
}

case class CProperty[A](name: String, features: Seq[Feature]) extends Property[A] {
  def set(feature: Feature): Property[A] = copy(features = this.features :+ feature)
}

case class PropertyValue[A](property: Property[A], value: A)

trait Composite[V] {
  def features: Seq[Feature] = Seq.empty[Feature]
  def property[A](name: String): Property[A] = CProperty[A](name, Seq.empty[Feature])
  def properties: Seq[Property[_]]
}

trait RefModel[R] extends Composite[R]

trait DataModel[D] extends Composite[D]

trait HasId[E, Id] extends HasProperties[E] { model: Model[E] =>
  def id: Property[Id] = CProperty[Id]("id", Seq.empty[Feature])
  //def parse(stringId: String): Try[Id]
  //def generate: Id
  abstract override def properties: Seq[Property[_]] = super.properties :+ id
}

trait CanParse[A] {
  def parse(string: String): Try[A]
}

trait CanGenerate[A] {
  def generate: A
}

trait HasCompositeId[E, Id] extends HasProperties[E] { model: Model[E] =>
  def id: Composite[Id]
  abstract override def properties: Seq[Property[_]] = super.properties ++ id.properties
}
trait HasRef[E, R] extends HasProperties[E] { model: Model[E] =>
  def ref: RefModel[R]
  abstract override def properties: Seq[Property[_]] = super.properties ++ ref.properties
}

trait HasData[E, D] extends HasProperties[E] { this: Model[E] =>
  def data: DataModel[D]
  abstract override def properties: Seq[Property[_]] = super.properties ++ data.properties
}

trait CanCombine0[E] {
  def combine: E
}

trait CanCombine1[E, A1] {
  def combine(a1: A1): E
}

trait CanCombine2[E, A1, A2] {
  def combine(a1: A1, a2: A2): E
}

trait CanCombine3[E, A1, A2, A3] {
  def combine(a1: A1, a2: A2, a3: A3): E
}

trait CanCombine4[E, A1, A2, A3, A4] {
  def combine(a1: A1, a2: A2, a3: A3, a4: A4): E
}

trait CanCombine5[E, A1, A2, A3, A4, A5] {
  def combine(a1: A1, a2: A2, a3: A3, a4: A4, a5: A5): E
}


trait Model[E] extends HasProperties[E] {
  def name: String
  def property[A](name: String): Property[A] = CProperty[A](name, Seq.empty[Feature])
  def properties: Seq[Property[_]] = Seq.empty[Property[_]]
  def features: Seq[Feature] = Seq.empty[Feature]
}




