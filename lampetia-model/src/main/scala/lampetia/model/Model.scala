package lampetia.model

import scala.language.implicitConversions
import scala.util.Try
import shapeless._

/**
 * @author Hossam Karim
 */

trait Feature extends Any

// O: owner type
// V: property values type
trait Property[O, V] {
  def name: String
  def set(instance: O, value: V): O
  def get(instance: O): V
  def set(feature: Feature): Property[O, V]
  def features: Seq[Feature]
}

case class CProperty[O, A](name: String, features: Seq[Feature], fg: O => A, fs: O => A => O) extends Property[O, A] {
  def get(instance: O): A = fg(instance)
  def set(instance: O, value: A): O = fs(instance)(value)
  def set(feature: Feature): CProperty[O, A] = copy(features = this.features :+ feature)
}

case class PropertyValue[O, A](property: Property[O, A], value: A)

trait Composite[O, V] {
  def set(instance: O, value: V): O
  def get(instance: O): V
  def properties: Seq[Property[V, _]]
  def features: Seq[Feature] = Seq.empty[Feature]

  def property[A](name: String, fg: V => A, fs: V => A => V): Property[V, A] =
    CProperty[V, A](name, Seq.empty[Feature], fg, fs)
}

trait RefModel[O, Ref] extends Composite[O, Ref]

trait DataModel[O, Data] extends Composite[O, Data]


trait HasProperties[O] {
  final type Properties = Seq[Property[_, _]]
  def properties: Properties
}

trait HasId[E, Id] extends HasProperties[E] { this: Model[E] =>
  def id: Property[E, Id]
  def parse(stringId: String): Try[Id]
  def generate: Id
  abstract override def properties: Properties = super.properties :+ id
}
trait HasRef[E, Ref] extends HasProperties[E] { this: Model[E] =>
  def ref: RefModel[E, Ref]
  abstract override def properties: Properties = super.properties ++ ref.properties
}

trait HasData[E, Data] extends HasProperties[E] { this: Model[E] =>
  def data: DataModel[E, Data]
  abstract override def properties: Properties = super.properties ++ data.properties
}



trait Model[E] extends HasProperties[E] {
  type Shape
  def name: String
  def property[A](name: String, fg: E => A, fs: E => A => E): Property[E, A] =
    CProperty[E, A](name, Seq.empty[Feature], fg, fs)

  def properties: Properties = Seq.empty[Property[_, _]]
  def features: Seq[Feature] = Seq.empty[Feature]

  def combine(hl: Shape): E

}




