package lampetia

import lampetia.extensions.Strings._

/**
 * @author Hossam Karim
 */
package object metamodel {

  trait Feature

  trait FeatureOwner {
    def features: Seq[Feature]
  }

  trait Keyword

  trait FeatureScope {
    type F <: Feature

    def baseScope: Option[FeatureScope] = None

    def feature(keyword: Keyword): F

    def features(fo: FeatureOwner): Seq[Feature]

    def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword]
  }

  trait Property extends FeatureOwner {
    type Self <: Property

    def propertyName: String

    def tpe: Model

    def <<(fs: Feature*): Self

    def <+(fs: Seq[Feature]): Self = <<(fs:_*)

    def optional: Boolean

    def list: Boolean

    def typeName: String =
      if (optional)
        s"Option[${tpe.modelName}]"
      else if (list)
        s"List[${tpe.modelName}]"
      else
        tpe.modelName
  }


  trait Model extends FeatureOwner {
    type Self <: Model

    def modelName: String

    def properties: Seq[Property]

    def <<(fs: Feature*): Self

    def <+(fs: Seq[Feature]): Self = <<(fs:_*)

  }

  trait SubmoduleType
  case object SubmoduleModel extends SubmoduleType
  case object SubmoduleService extends SubmoduleType

  sealed trait ModuleOption
  case object Secure extends ModuleOption
  case object NonSecure extends ModuleOption
  case object EnableTestingFeatures extends ModuleOption
  case class BaseGenerationDirectory(path: String) extends ModuleOption


  case class Module(
    name: String,
    basePackage: String,
    databaseSchema: String,
    features: Seq[Feature] = Nil,
    options: Seq[ModuleOption] = Seq.empty[ModuleOption],
    mn: Option[String]= None) extends Model {
    type Self = Module
    val modelName = mn.getOrElse(name)
    val properties = Nil
    def <<(fs: Feature*): Module = copy(features = fs ++ features)
    def secure: Boolean = options.exists {
      case Secure => true
      case _      => false
    }

    def enableTestingFeatures: Boolean = options.exists {
      case EnableTestingFeatures => true
      case _                     => false
    }

  }



  case class Submodule(parent: Module, submoduleType: SubmoduleType)

  case class NameKeyword(value: String) extends Keyword

  sealed trait NameFormatKeyword extends Keyword

  case object LispCase extends NameFormatKeyword

  case object SnakeCase extends NameFormatKeyword

  case object CamelCase extends NameFormatKeyword

  sealed trait JsonCompositeKeyword extends Keyword
  case object JsonComposite extends JsonCompositeKeyword
  case object JsonbComposite extends JsonCompositeKeyword
  case object JsonArrayComposite extends JsonCompositeKeyword
  case object JsonbArrayComposite extends JsonCompositeKeyword

  case class Flatten(names: Map[String, String]) extends Keyword

  case class Index(picker: String => Boolean, unique: Boolean = false) extends Keyword

  case class Imports(templateKey: String, imports: Seq[String]) extends Keyword

  case class DependsOn(submodule: Submodule) extends Keyword

  sealed trait JsonType extends Model {
    def properties: Seq[Property] = Nil
    def features: Seq[Feature] = Nil
  }

  case object Jsonp extends JsonType {
    type Self = Jsonp.type
    def modelName = "Json"
    def <<(fs: Feature*): Jsonp.type = this
  }

  case object JsonData extends JsonType {
    type Self = JsonData.type
    def modelName = "JsonData"
    def <<(fs: Feature*): JsonData.type = this
  }

  case class JsonProperty
  (propertyName: String, features: Seq[Feature] = Nil, optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = JsonProperty
    def tpe: Model = Jsonp
    def <<(fs: Feature*): JsonProperty = copy(features = fs ++ features)
  }

  case class JsondProperty
  (propertyName: String, features: Seq[Feature] = Nil, optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = JsondProperty
    def tpe: Model = JsonData
    def <<(fs: Feature*): JsondProperty = copy(features = fs ++ features)
  }

  case object JsonbData extends JsonType {
    type Self = JsonbData.type
    def modelName = "JsonData"
    def <<(fs: Feature*): JsonbData.type = this
  }
  case class JsonbProperty
  (propertyName: String, features: Seq[Feature] = Nil, optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = JsonbProperty
    def tpe: Model = JsonbData
    def <<(fs: Feature*): JsonbProperty = copy(features = fs ++ features)
  }

  sealed trait LiteralType extends Model {

    def literalTypeName: String

    def modelName = literalTypeName

    def properties: Seq[Property] = Nil

    def features: Seq[Feature] = Nil
  }

  case object StringLiteral extends LiteralType with Model {
    type Self = StringLiteral.type
    def literalTypeName = "String"
    def <<(fs: Feature*): StringLiteral.type = this
  }

  case object IntLiteral extends LiteralType {
    type Self = IntLiteral.type
    def literalTypeName = "Int"
    def <<(fs: Feature*): IntLiteral.type = this
  }

  case object BooleanLiteral extends LiteralType {
    type Self = BooleanLiteral.type
    def literalTypeName = "Boolean"
    def <<(fs: Feature*): BooleanLiteral.type = this
  }

  case object DateTimeLiteral extends LiteralType {
    type Self = DateTimeLiteral.type
    def literalTypeName = "DateTime"
    def <<(fs: Feature*): DateTimeLiteral.type = this
  }

  case object JsValueLiteral extends LiteralType {
    type Self = JsValueLiteral.type
    def literalTypeName = "JsValue"
    def <<(fs: Feature*): JsValueLiteral.type = this
  }

  case class LiteralProperty
  (propertyName: String, tpe: LiteralType, optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = LiteralProperty
    def features = Nil
    def <<(fs: Feature*): LiteralProperty = this
  }

  case class Value(modelName: String, valueProperty: Property, features: Seq[Feature] = Nil, convertedFrom: Option[Id] = None) extends Model {
    type Self = Value
    def properties = Seq(valueProperty)
    def <<(fs: Feature*): Value = this.copy(features = fs ++ features)
  }

  case class ValueProperty
  (propertyName: String, tpe: Value, features: Seq[Feature] = Nil, optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = ValueProperty
    def <<(fs: Feature*): ValueProperty = copy(features = fs ++ features)
  }

  case class Composite(modelName: String, properties: Seq[Property], features: Seq[Feature]) extends Model {
    type Self = Composite
    def <<(fs: Feature*): Composite = copy(features = fs ++ features)
  }

  case class CompositeProperty
  (propertyName: String, tpe: Composite, features: Seq[Feature], optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = CompositeProperty
    def <<(fs: Feature*): CompositeProperty = copy(features = fs ++ features)
  }

  case class Id(entity: Entity, features: Seq[Feature]) extends Model {
    type Self = Id
    val modelName = s"${entity.modelName}Id"
    def properties = Nil
    def <<(fs: Feature*): Id = copy(features = fs ++ features)
  }

  case class IdProperty(tpe: Id, features: Seq[Feature] = Nil) extends Property {
    type Self = IdProperty
    val propertyName = "id" //s"${tpe.modelName.camelCase}"
    def <<(fs: Feature*): IdProperty = copy(features = fs ++ features)
    def optional = false
    def list = false
  }
  case class RefProperty
      (name: Option[String],
       reference: IdProperty,
       features: Seq[Feature] = Nil,
       optional: Boolean = false,
       parent: Boolean = false) extends Property {
    type Self = RefProperty
    val tpe = reference.tpe
    val propertyName = name.getOrElse(s"${tpe.modelName.camelCase}")
    def <<(fs: Feature*): RefProperty = copy(features = fs ++ features)
    def list = false
  }


  case class Entity(
    modelName: String,
    refAndDataProperties: Seq[Property],
    resourceType: String,
    features: Seq[Feature]) extends Model {
    type Self = Entity
    val id: IdProperty = IdProperty(Id(this, Nil), Nil)
    def properties = Seq(id) ++ refAndDataProperties
    def withResourceType(rt: String) = copy(resourceType = rt)
    def <<(fs: Feature*): Entity = copy(features = fs ++ features)
  }

  case class Enum(
                   modelName: String,
                   cases: Seq[EnumCase],
                   generalCase: Option[EnumCase] = None,
                   discriminator: LiteralProperty = LiteralProperty("name", StringLiteral),
                   features: Seq[Feature]) extends Model {

    type Self = Enum
    def withDiscriminator(property: LiteralProperty): Enum = this.copy(discriminator = property)
    def withGeneralCase(gcase: EnumCase): Enum = this.copy(generalCase = Some(gcase))
    def properties = Nil
    def <<(fs: Feature*): Enum = copy(features = fs ++ features)
  }

  case class EnumCase(name: String, value: String)

  case class EnumProperty
  (propertyName: String, tpe: Enum, features: Seq[Feature], optional: Boolean = false, list: Boolean = false) extends Property {
    type Self = EnumProperty
    def <<(fs: Feature*): EnumProperty = copy(features = fs ++ features)
  }







}
