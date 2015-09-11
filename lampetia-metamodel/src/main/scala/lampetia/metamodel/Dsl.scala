package lampetia.metamodel

import scala.annotation.implicitNotFound
import lampetia.extensions.Strings._

/**
 * @author Hossam Karim
 */


@implicitNotFound("${FS} does not support Keywords of type ${K}")
trait ScopedKeyword[FS <: FeatureScope, K <: Keyword] {
  def feature(scope: FS, keyword: Keyword): FS#F
}


@implicitNotFound("Cannot build a Model from type ${A}")
trait ModelBuilder[A, M <: Model] {
  def build(name: String, partial: A): M
}

@implicitNotFound("Cannot build a Property from type ${M}")
trait PropertyBuilder[M <: Model, P <: Property] {
  def build(name: String, model: M, optional: Boolean = false, list: Boolean = false): P
}


trait Dsl {

  implicit class KeywordBuilder[K <: Keyword](keyword: K) {
    def in[FS <: FeatureScope](scope: FS)(implicit ev: ScopedKeyword[FS, K]): FS#F =
      ev.feature(scope, keyword)
  }

  implicit class Strings(s: String) {

    def is[A, M <: Model](partial: A)(implicit ev: ModelBuilder[A, M]): M =
      ev.build(s, partial)

    def of[M <: Model, P <: Property](model: M)(implicit ev: PropertyBuilder[M, P]): P =
      ev.build(s, model)

    def optionOf[M <: Model, P <: Property](model: M)(implicit ev: PropertyBuilder[M, P]): P =
      ev.build(name = s, model = model, optional = true)

    def listOf[M <: Model, P <: Property](model: M)(implicit ev: PropertyBuilder[M, P]): P =
      ev.build(name = s, model = model, list = true)

    def ref(property: IdProperty): RefProperty =
      RefProperty(Some(s), property, Nil)

    def parentRef(property: IdProperty): RefProperty =
      RefProperty(Some(s), property, Nil, optional = false, parent = true)

    def optionalRef(property: IdProperty): RefProperty =
      RefProperty(name = Some(s), reference = property, features = Nil, optional = true)

  }

  implicit class ModuleEx(module: Module) {
    def model = Submodule(module, SubmoduleModel)
    def service = Submodule(module, SubmoduleService)
    def organization(sub: SubmoduleType): Option[String] = None
    def artifactId(sub: SubmoduleType) = sub match {
      case SubmoduleModel   => s"${module.name.lispCase}-model"
      case SubmoduleService => s"${module.name.lispCase}-service"
      case _                => module.name
    }
    def version(sub: SubmoduleType): Option[String] = None
  }


  implicit def literalPropertyBuilder[L <: LiteralType]: PropertyBuilder[L, LiteralProperty] =
    new PropertyBuilder[L, LiteralProperty] {
      def build(name: String, model: L, optional: Boolean = false, list: Boolean = false): LiteralProperty =
        LiteralProperty(name, model, optional, list)
    }

  implicit class StringToLiteralProperty(s: String) {
    def string = LiteralProperty(s, StringLiteral)
    def int = LiteralProperty(s, IntLiteral)
    def double = LiteralProperty(s, DoubleLiteral)
    def boolean = LiteralProperty(s, BooleanLiteral)
    def dateTime = LiteralProperty(s, DateTimeLiteral)
    def jsvalue = LiteralProperty(s, JsValueLiteral)
    def json = JsonProperty(s)
    def jsond = JsondProperty(s)
    def jsonb = JsonbProperty(s)
  }



  def ref(property: IdProperty): RefProperty =
    RefProperty(None, property, Nil)

  def optionalRef(property: IdProperty): RefProperty =
    RefProperty(name = None, reference = property, features = Nil, optional = true)

  def value(name: String)(valueProperty: Property): Value = Value(name, valueProperty, Nil)

  implicit val valuePropertyBuilder = new PropertyBuilder[Value, ValueProperty] {
    def build(name: String, model: Value, optional: Boolean = false, list: Boolean = false): ValueProperty =
      ValueProperty(name, model, Nil, optional, list)
  }

  def composite(name: String)(properties: Property*): Composite = Composite(name, properties, Nil)
  def composite(properties: Property*): String => Composite = composite(_)(properties:_*)

  implicit val compositeBuilder = new ModelBuilder[String => Composite, Composite] {
    def build(name: String, partial: (String) => Composite): Composite =
      partial(name)
  }
  implicit val compositePropertyBuilder = new PropertyBuilder[Composite, CompositeProperty] {
    def build(name: String, model: Composite, optional: Boolean = false, list: Boolean = false): CompositeProperty =
      CompositeProperty(name, model, Nil, optional, list)
  }


  def enum(name: String)(cases: EnumCase*): Enum = Enum(name, cases = cases, features = Nil)
  def enum(cases: EnumCase*): String => Enum = enum(_)(cases:_*)

  def enumCase(name: String): EnumCase = EnumCase(name, name.toUpperCase)
  def enumCase(name: String, value: String): EnumCase = EnumCase(name, value)

  implicit class enumCaseBuilder(s: String) {
    def ecase = EnumCase(s, s.toUpperCase)
  }

  implicit val enumPropertyBuilder = new PropertyBuilder[Enum, EnumProperty] {
    def build(name: String, model: Enum, optional: Boolean = false, list: Boolean = false): EnumProperty =
      EnumProperty(name, model, Nil, optional, list)
  }


  def entity(name: String, resourceType: Option[String] = None)(properties: Property*): Entity =
    resourceType match {
      case Some(rt) => Entity(name, properties, rt, Nil)
      case None => Entity(name, properties, s"${name.lispCase}", Nil)
    }

  implicit val entityPropertyBuilder = new PropertyBuilder[Entity, EntityProperty] {
    def build(name: String, model: Entity, optional: Boolean = false, list: Boolean = false): EntityProperty =
      EntityProperty(name, model, Nil, optional, list)
  }

  def extend(entity: Entity)(additional: Property*): Entity =
    entity.copy(refAndDataProperties = entity.refAndDataProperties ++ additional)

  def entity(properties: Property*): String => Entity =  entity(_)(properties:_*)
}

trait DslKeywordBuilder {
  this: Dsl =>

  def named(value: String) = NameKeyword(value)

  def lispCase = LispCase

  def snakeCase = SnakeCase

  def camelCase = CamelCase

  def imports(templateKey: String, pkgs: String*): Imports =
    Imports(templateKey, pkgs)

  def flatten(names: Map[String, String] = Map()) = Flatten(names)
  def flatten(names: (String,String)*) = Flatten(Map(names:_*))

  def index(picker: String => Boolean) = Index(picker)
  def uniqueIndex(picker: String => Boolean) = Index(picker, unique = true)

  def dependsOn(submodule: Submodule) = DependsOn(submodule)

  def jsonComposite = JsonComposite
  def jsonbComposite = JsonbComposite
  def jsonArrayComposite = JsonArrayComposite
  def jsonbArrayComposite = JsonbArrayComposite


}

object Dsl extends Dsl with DslKeywordBuilder {

  object Template extends TemplateScope with TemplateScopeImplicits

  object ModelScope extends ModelScope with ModelScopeImplicits
  object ServiceScope extends ServiceScope with ServiceScopeImplicits

  object Sql extends SqlScope with SqlScopeImplicits

  object Json extends JsonScope with JsonScopeImplicits
  object JsonReader extends JsonReaderScope with JsonReaderScopeImplicits
  object JsonWriter extends JsonWriterScope with JsonWriterScopeImplicits

  def commonFeatures = Seq(snakeCase in Sql, lispCase in Json)

  val ResourceType = value("ResourceType")("value".string)
  val ResourceId = value("ResourceId")("value".string)
  val Resource = composite("Resource")("id" of ResourceId, "resourceType" of ResourceType)
  val SubjectId = value("SubjectId")("value".string)
  val User = entity("User")() <+ commonFeatures << (named("security_user") in Sql)
  val Name = value("Name")("value".string)
  val Title = value("Title")("value".string)
  val Email = value("Email")("value".string)
  val Timestamp =
    composite("Timestamp")("createdAt".dateTime, "updatedAt".dateTime) <+ commonFeatures

  implicit class StringToCommon(s: String) {
    def resourceType = s of ResourceType
    def resourceId = s of ResourceId
    def resource = s of Resource
    def subjectId = s of SubjectId
    def userId = s ref User.id
    def name = s of Name
    def title = s of Title
    def email = s of Email
    def timestamp = (s of Timestamp) << (Flatten(Map()) in Sql)
  }

  def idToValue(idModel: Id): Value =
    Value(idModel.modelName, "value".string, Nil, Some(idModel)) <+ commonFeatures

}
