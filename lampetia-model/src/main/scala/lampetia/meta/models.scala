package lampetia.meta

import org.joda.time.DateTime

import scala.util.Try

/**
 * @author Hossam Karim
 */

trait Feature extends Any

sealed trait PropertyType[+A]
case object IntProperty extends PropertyType[Int]
case object FloatProperty extends PropertyType[Float]
case object DoubleProperty extends PropertyType[Double]
case object LongProperty extends PropertyType[Long]
case object StringProperty extends PropertyType[String]
case object DateProperty extends PropertyType[DateTime]
case object DefaultProperty extends PropertyType[Nothing]

trait Property[V] {
  def propertyName: String
  def propertyType: PropertyType[V]
  def set(feature: Feature): Property[V]
  def features: Seq[Feature]
}

trait Readable[E, V] {
  def get(instance: E): V
}

trait Writeable[E, V] {
  def set(instance: E, value: V): E
}

trait Lens[E, V] extends Readable[E, V] with Writeable[E, V]

trait HasProperties[A] {
  def properties: Seq[Property[_]]
}

case class SimpleProperty[A](propertyName: String, propertyType: PropertyType[A], features: Seq[Feature]) extends Property[A] {
  def set(feature: Feature): SimpleProperty[A] = copy(features = this.features :+ feature)
  def getter[E](f: E => A): ReadableProperty[E, A] =
    ReadableProperty[E, A](propertyName, propertyType, features, f)
  def setter[E](f: (E, A) => E): WriteableProperty[E, A] =
    WriteableProperty[E, A](propertyName, propertyType, features, f)
}
case class ReadableProperty[E, A](propertyName: String, propertyType: PropertyType[A], features: Seq[Feature], reads: E => A)
  extends Property[A] with Readable[E, A] {
  def set(feature: Feature): ReadableProperty[E, A] = copy(features = this.features :+ feature)
  def get(instance: E): A = reads(instance)
  def setter(f: (E, A) => E): LensProperty[E, A] =
    LensProperty[E, A](propertyName, propertyType, features, reads, f)
}
case class WriteableProperty[E, A](propertyName: String, propertyType: PropertyType[A], features: Seq[Feature], writes: (E, A) => E)
  extends Property[A] with Writeable[E, A] {
  def set(feature: Feature): WriteableProperty[E, A] = copy(features = this.features :+ feature)
  def set(instance: E, value: A): E = writes(instance, value)
  def getter(f: E => A): LensProperty[E, A] =
    LensProperty[E, A](propertyName, propertyType, features, f, writes)
}
case class LensProperty[E, A](propertyName: String, propertyType: PropertyType[A], features: Seq[Feature], reads: E => A, writes: (E, A) => E)
  extends Property[A] with Lens[E, A] {
  def set(feature: Feature): LensProperty[E, A] = copy(features = this.features :+ feature)
  def get(instance: E): A = reads(instance)
  def set(instance: E, value: A): E = writes(instance, value)
}

//case class PropertyValue[A](property: Property[A], value: A)

trait Composite[V] {
  def features: Seq[Feature] = Seq.empty[Feature]
  def property[A](name: String)(implicit pt: PropertyType[A]) =
    SimpleProperty[A](name, pt, Seq.empty[Feature])
  def properties: Seq[Property[_]]
}

trait RefModel[R] extends Composite[R]

trait DataModel[D] extends Composite[D]

trait HasId[E, Id] extends HasProperties[E] { model: Model[E] =>
  def id: Property[Id]
  abstract override def properties: Seq[Property[_]] = super.properties :+ id
}

trait CanParse[A] {
  def parse(string: String): Try[A]
}

trait Stringify[A] {
  def stringify(instance: A): String
}

trait CanGenerate[A] {
  def generate: A
}

trait HasCompositeId[E, Id] extends HasProperties[E] { model: Model[E] =>
  def id: Composite[Id]
  abstract override def properties: Seq[Property[_]] = super.properties ++ id.properties
}

trait HasComposite[E] extends HasProperties[E] { model: Model[E] =>
  def composite: Composite[_]
  abstract override def properties: Seq[Property[_]] = super.properties ++ composite.properties
}

trait HasRef[E, R] extends HasProperties[E] { model: Model[E] =>
  def ref: RefModel[R]
  abstract override def properties: Seq[Property[_]] = super.properties ++ ref.properties
}

trait HasData[E, D] extends HasProperties[E] { this: Model[E] =>
  def data: DataModel[D]
  abstract override def properties: Seq[Property[_]] = super.properties ++ data.properties
}



trait Model[E] extends HasProperties[E] {

  implicit val intProperty = IntProperty
  implicit val floatProperty = FloatProperty
  implicit val doubleProperty = DoubleProperty
  implicit val longProperty = LongProperty
  implicit val stringProperty = StringProperty
  implicit val dateProperty = DateProperty
  implicit def anyProperty[A]: PropertyType[A] = DefaultProperty

  def modelName: String
  def property[A](name: String)(implicit pt: PropertyType[A]): SimpleProperty[A] =
    SimpleProperty[A](name, pt, Seq.empty[Feature])
  def properties: Seq[Property[_]] = Seq.empty[Property[_]]
  def features: Seq[Feature] = Seq.empty[Feature]
}









