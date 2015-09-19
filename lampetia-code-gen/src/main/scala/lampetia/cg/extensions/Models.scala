package lampetia.cg.extensions

import java.util.UUID

import lampetia.metamodel.Dsl._
import lampetia.metamodel._
import lampetia.extensions.Strings._

import scala.util.Random

/**
 * @author Hossam Karim
 */


trait Models {

  def postgresqlJsonType: String
  def postgresqlJsonbType: String
  def postgresqlJsonMappedType: String
  def postgresqlJsonbMappedType: String

  def namer(original: String): PartialFunction[Keyword, String] = {
    case NameKeyword(n) => n
    case LispCase => original.lispCase
    case SnakeCase => original.snakeCase
    case CamelCase => original.camelCase
  }

  def formater(original: String): PartialFunction[Keyword, String] = {
    case LispCase => original.lispCase
    case SnakeCase => original.snakeCase
    case CamelCase => original.camelCase
  }

  implicit class ModuleEx(module: Module) {
    def sqlFormatMixins: String =
      ServiceScope.keywordsIn(module).collect {
        case DependsOn(sub) =>
          s"${sub.parent.name}SqlFormat"
      }.toSet.toList match {
        case Nil      => ""
        case x :: Nil => s" with $x"
        case xs       => s" with ${xs.mkString(" with ")}"
      }
  }

  implicit class ModelsEx[M <: Model](models: Seq[M]) {

    def valueModels: Seq[Value] = models.collect { case m: Value => m}

    def compositeModels: Seq[Composite] = models.collect { case m: Composite => m}

    def entityModels: Seq[Entity] = models.collect { case m: Entity => m}

    def entityRefModels: Seq[Composite] =
      entityModels.filter(_.hasReferenceModel).map(_.referenceModel)

    def enumModels: Seq[Enum] = models.collect { case m: Enum => m}

    def jsonModels: Seq[M] = models

    def messagePackModels: Seq[M] = models

    def sqlModels: Seq[Entity] = entityModels

    def idModels: Seq[Id] =
      models.collect { case m: Id => m }

    def idEntity: Seq[Entity] =
      models.collect { case m: Id => m }.map(_.entity)

    def seqProperties: Set[Model] =models.map(_.properties).flatten.filter(_.list).map(_.tpe).filter(_.isInstanceOf[Model]).toSet
    def optionalProperties: Set[Model] =models.map(_.properties).flatten.filter(_.optional).map(_.tpe).filter(pm => pm.isInstanceOf[Model] && !pm.isInstanceOf[LiteralType]).toSet

  }

  implicit class ModelEx[M <: Model](model: M) {
    def generateInstance: String = model match {
      case m: Id =>
        val id = s"UUID.randomUUID().toString"
        s"${m.modelName}($id)"
      case m: Value => ValueEx(m).generateInstance
      case m: Composite => CompositeEx(m).generateInstance
      case m: Entity => EntityEx(m).generateInstance
      case m: Enum => EnumEx(m).generateInstance
      case m: LiteralType => m.literalTypeName match {
        case "String" => Random.alphanumeric.take(10).toList.foldLeft("")(_ + _).qoute
        case "Int" => Random.nextInt().toString
        case "Double" => Random.nextDouble().toString
        case "Boolean" => Random.nextBoolean().toString
        case "DateTime" => "DateTime.now"
        case "JsValue" => "JsValue"
      }
      case m: JsonType =>
        s"""JsonData(Json.obj("id" -> "some-id", "data" -> List("something", "something else")))"""
    }

    def external: Boolean =
      model.features.collectFirst {
        case e: ModelFeature => e.keyword match {
          case External => true
          case _ => false
        }
      }.getOrElse(false)

    def createParameter: String = model match {
      case e: Entity => e.createParameter
      case _ => "/*TODO : not yet implemented*/"
    }
  }

  implicit class PropertyEx(p: Property) {
    def propertyTypeInEntity = p.typeName
    def nameColonTypeInEntity = s"${p.propertyName}: ${p.propertyTypeInEntity}"
    def generateNameValue = p match {
      case _ if p.list =>
        val instances = (0 to Random.nextInt(5)).map(_ => p.tpe.generateInstance)
        s"${p.propertyName} = Seq(${instances.mkString(", ")})"
      case _ if p.optional =>
        s"${p.propertyName} = Some(${p.tpe.generateInstance})"
      case _ =>
        s"${p.propertyName} = ${p.tpe.generateInstance}"
    }
  }

  implicit class SqlModelEx[M <: Model](model: M) {

    def sqlName: String =
      Sql.keywordsIn(model)
        .collectFirst(namer(model.modelName))
        .getOrElse(model.modelName)

    def metaName: String = s"${model.modelName}Meta"

  }

  implicit class JsonModelEx[M <: Model](model: M) {
    def jsonName: String =
      Json.keywordsIn(model)
        .collectFirst(namer(model.modelName))
        .getOrElse(model.modelName)

    def jsonReaderName: String =
      Json.keywordsIn(model)
        .collectFirst(namer(model.modelName))
        .getOrElse(jsonName)
  }

  implicit class SqlPropertyEx[P <: Property](property: P) {
    def sqlNameOption: Option[String] =
      Sql.keywordsIn(property)
        .collectFirst(namer(sqlFlattenName))

    def sqlNameWithParent(m: Model): String =
      Sql.keywordsIn(m)
        .collectFirst(formater(property.propertyName))
        .getOrElse(property.propertyName)

    def sqlName(m: Model) =
      sqlNameOption getOrElse sqlNameWithParent(m)

    def sqlFlatten: Boolean =
      property.features.collectFirst {
        case SqlFeature(Flatten(_)) => true
      }.getOrElse(false)

    def sqlFlattenName: String =
      property.features.collectFirst {
        case SqlFeature(Flatten(m)) if m.get(property.propertyName).nonEmpty =>
          m(property.propertyName)
      }.getOrElse(property.propertyName)

    def sqlType: String = property match {
      case p: JsondProperty => postgresqlJsonMappedType
      case p: JsonbProperty => postgresqlJsonbMappedType
      case _ => property.propertyTypeInEntity
    }

    def sqlRecordProperty = property match {
      case p: JsondProperty => s"${property.propertyName}: $postgresqlJsonMappedType"
      case p: JsonbProperty => s"${property.propertyName}: $postgresqlJsonbMappedType"
      case p => s"${p.sqlFlattenName}: ${p.propertyTypeInEntity}"
    }

    def propertyCouple(m: Model) = property match {
      case p: Property if p.sqlJsonb =>
        s"""bind.cast(Types.jsonb)"""
      case _ =>
        s"""bind"""
    }

    private def defaultSqlColumnProperty(m: Model) = property match {
      case p if p.optional =>
        s"""val $sqlFlattenName = property[$sqlType](\"${property.propertyName}\").set(sql.optional)"""
      case p if p.sqlJsonb =>
        s"""val $sqlFlattenName = property[$sqlType](\"${property.propertyName}\").set(sql.`type`("jsonb"))"""
      case _ =>
        s"""val $sqlFlattenName = property[$sqlType](\"${property.propertyName}\")"""
    }

    private def mkType(inner: String) = property match {
      case p if p.optional => s"Option[$inner]"
      case p if p.list => s"Seq[$inner]"
      case p => inner
    }

    def sqlNeedsJsonListMapping: Boolean = property match {
      case _: CompositeProperty =>
        Sql.keywordsIn(property).collectFirst {
          case JsonComposite | JsonbComposite => true
        }.getOrElse(false)
      case _ => false
    }

    def sqlJsonb: Boolean = property match {
      case _ =>
        Sql.keywordsIn(property).collectFirst {
          case JsonbComposite => true
        }.getOrElse(false)
    }


    def sqlJsonType: String =
      Sql.keywordsIn(property).collectFirst {
        case JsonComposite => postgresqlJsonMappedType
        case JsonbComposite => postgresqlJsonbMappedType
      }.getOrElse("")

    def sqlColumnProperty(m: Model) = property match {
      case _: JsondProperty =>
        s"""val $sqlFlattenName = property[${mkType(postgresqlJsonMappedType)}](\"${property.propertyName}\"))"""

      case _: JsonbProperty =>
        s"""val $sqlFlattenName = property[${mkType(postgresqlJsonbMappedType)}](\"${property.propertyName}\")).set(sql.`type`("jsonb"))"""

      case _: CompositeProperty =>
        Sql.keywordsIn(property).collectFirst {
          case JsonComposite | JsonArrayComposite =>
            s"""val $sqlFlattenName = property[${mkType(property.tpe.modelName)}](\"${property.propertyName}\")"""
          case JsonbComposite | JsonbArrayComposite =>
            s"""val $sqlFlattenName = property[${mkType(property.tpe.modelName)}](\"${property.propertyName}\")"""
        }.getOrElse(defaultSqlColumnProperty(m))

      case _ => defaultSqlColumnProperty(m)
    }


  }

  implicit class JsonPropertyEx[P <: Property](property: P) {
    def jsonNameOption: Option[String] =
      Json.keywordsIn(property)
        .collectFirst(namer(property.propertyName))

    def jsonReaderNameOption: Option[String] =
      JsonReader.keywordsIn(property)
        .collectFirst(namer(property.propertyName))

    def jsonWriterNameOption: Option[String] =
      JsonWriter.keywordsIn(property)
        .collectFirst(namer(property.propertyName))

    def jsonNameWithParent(m: Model): String =
      Json.keywordsIn(m)
        .collectFirst(formater(property.propertyName))
        .getOrElse(property.propertyName)

    def jsonReaderNameWithParent(m: Model): Option[String] =
      JsonReader.keywordsIn(m)
        .collectFirst(formater(property.propertyName))

    def jsonWriterNameWithParent(m: Model): Option[String] =
      JsonWriter.keywordsIn(m)
        .collectFirst(formater(property.propertyName))

    def jsonName(m: Model) =
      jsonNameOption getOrElse jsonNameWithParent(m)

    def jsonReaderName(m: Model) =
      jsonReaderNameOption.getOrElse(jsonReaderNameWithParent(m).getOrElse(jsonName(m)))

    def jsonWriterName(m: Model) =
      jsonWriterNameOption.getOrElse(jsonWriterNameWithParent(m).getOrElse(jsonName(m)))

    def jsonWriter(instanceName: String, m: Model) =
      s""""${jsonWriterName(m)}" -> $instanceName.${property.propertyName}"""

    def jsonReader(m: Model) = property match {
      case id: IdProperty =>
        s"optionalId.map(${id.tpe.modelName})"

      case ref: RefProperty if ref.optional =>
        m match {
          case _: Entity =>
            s"""(JsPath \\ "owner" \\ "${jsonReaderName(m)}").readNullable[String].map(_.map(${ref.tpe.modelName}))"""
          case _ =>
            s"""(JsPath \\ "${jsonReaderName(m)}").readNullable[String].map(_.map(${ref.tpe.modelName}))"""
        }

      case ref: RefProperty =>
        m match {
          case _: Entity =>
            s"""(JsPath \\ "owner" \\ "${jsonReaderName(m)}").read[String].map(${ref.tpe.modelName})"""
          case _ =>
            s"""(JsPath \\ "${jsonReaderName(m)}").read[String].map(${ref.tpe.modelName})"""
        }

      case p               => s"""(JsPath \\ "${jsonReaderName(m)}").read[${p.typeName}]"""
    }

    def jsonComposite: Boolean =
      property match {
        case cmp: CompositeProperty =>
          Json.keywordsIn(cmp.tpe).collectFirst {
            case JsonComposite | JsonbComposite => true
          }.getOrElse(false)
        case _ => false
      }


  }

  implicit class SqlPropertiesEx(properties: Seq[Property]) {
    // we support Flattening a composite model into several properties, let's create them
    def sqlProperties(m: Model): Seq[Property] = properties.flatMap {
      case p: CompositeProperty if p.sqlFlatten =>
        // make sure to preserve the features of the original expanded property along with the owner model features
        p.tpe.properties.flatMap {
          case composite: CompositeProperty if p.sqlFlatten =>
            composite.tpe.properties.sqlProperties(m)
          case nonComposite                                 =>
            Seq(nonComposite <+ p.features <+ m.features)
        }
      case p => Seq(p)
    }
  }

  // we might add support for altering the properties in Json later, hence:
  implicit class JsonPropertiesEx(properties: Seq[Property]) {
    def jsonReaderProperties: Seq[Property] = properties
    def jsonWriterProperties: Seq[Property] = properties
  }

  implicit class ValueEx(model: Value) {
    def generateInstance: String = {
      s"${model.modelName}(${model.properties.map(_.generateNameValue).mkString(",\n")})"
    }
  }

  implicit class CompositeEx(model: Composite) {

    def refModelName = s"${model.modelName}Ref"

    def generateInstance: String = {
      s"${model.modelName}(${model.properties.map(_.generateNameValue).mkString(",\n")})"
    }

    def needsSqlJsonMapper: Boolean =
      Sql.keywordsIn(model).collectFirst {
        case JsonComposite | JsonbComposite  => true
      }.getOrElse(false)

    def sqlJsonMapper: String = {

      def mapper(jsonType: String) =
        s"""
          |// Composite ${model.modelName} $jsonType Mapping
          |implicit val ${model.modelName.camelCase}Mapper: BaseColumnType[${model.modelName}] =
          |  MappedColumnType.base[${model.modelName}, $jsonType](
          |      in => $jsonType(JsonData(implicitly[Writes[${model.modelName}]].writes(in))),
          |      json => implicitly[Reads[${model.modelName}]].reads(json.value.value).get
          |  )
          |
          |// Composite ${model.modelName} GetResult
          |implicit val ${model.modelName.camelCase}GetResult = GetResult[${model.modelName}] { r =>
          |  val json: JsValue = Json.parse(r.<<[String])
          |  implicitly[Reads[${model.modelName}]].reads(json).get
          |}
          |
          |// Composite ${model.modelName} SetParameter
          |implicit val ${model.modelName.camelCase}SetParameter = SetParameter[${model.modelName}]{ (data, pp) =>
          |  val json = JsonData(implicitly[Writes[${model.modelName}]].writes(data))
          |  pp >> json
          |}
        """.stripMargin

      Sql.keywordsIn(model).collectFirst {
        case JsonComposite  => mapper(postgresqlJsonMappedType)
        case JsonbComposite => mapper(postgresqlJsonbMappedType)
      }.getOrElse("")

    }

    def needsSqlJsonArrayMapper: Boolean =
      Sql.keywordsIn(model).collectFirst {
        case JsonArrayComposite | JsonbArrayComposite  => true
      }.getOrElse(false)

    def sqlJsonArrayMapper: String = {

      def mapper(jsonType: String) =
        s"""
          |implicit val ${model.modelName.camelCase}ListMapper: BaseColumnType[List[${model.modelName}]] =
          |  MappedColumnType.base[List[${model.modelName}], $jsonType](
          |      in => $jsonType(JsonData(implicitly[Writes[List[${model.modelName}]]].writes(in))),
          |      json => implicitly[Reads[List[${model.modelName}]]].reads(json.value.value).get
          |  )
          |
          |implicit val ${model.modelName.camelCase}ListGetResult: GetResult[List[${model.modelName}]] = GetResult[List[${model.modelName}]] { r =>
          |  val json = Json.parse(r.<<?[String].getOrElse("[]"))
          |  implicitly[Reads[List[${model.modelName}]]].reads(json).get
          |}
          |
        """.stripMargin

      Sql.keywordsIn(model).collectFirst {
        case JsonArrayComposite  => mapper(postgresqlJsonMappedType)
        case JsonbArrayComposite => mapper(postgresqlJsonbMappedType)
      }.getOrElse("")

    }



  }

  implicit class EnumEx(enum: Enum) {

    def generateInstance: String = {
      enum.cases.map(_.name).head.toString
    }
  }

  implicit class EntityEx(model: Entity) {

    def entityRefProperties: Seq[RefProperty] =
      model.refAndDataProperties.collect { case rp: RefProperty => rp }

    def entityDataProperties: Seq[Property] =
      model.refAndDataProperties diff entityRefProperties

    def nonIdEntityProperties: Seq[Property] =
      entityRefProperties ++ entityDataProperties  ++ Seq(("timestamp" of Timestamp) << (Flatten(Map()) in Sql))

    def entityProperties: Seq[Property] =
      Seq(model.id) ++ nonIdEntityProperties

    def entitySqlProperties: Seq[Property] =
      entityProperties.sqlProperties(model)

    def nonIdEntitySqlProperties: Seq[Property] =
      nonIdEntityProperties.sqlProperties(model)


    def entityChildren(models: Seq[Entity]): Seq[Entity] = {
      models.filter(candidate => candidate.entityRefProperties.exists(_.reference.tpe.entity == model)).toSet.toSeq
    }

    def entityJsonArrayComposites: Set[Composite] =
      entityDataProperties.map(_.tpe).collect { case m: Composite => m }.filter { m =>
        Sql.keywordsIn(m).collectFirst {
          case JsonArrayComposite => true
        }.getOrElse(false)
      }.toSet

    def entityJsonbArrayComposites: Set[Composite] =
      entityDataProperties.map(_.tpe).collect { case m: Composite => m }.filter { m =>
        Sql.keywordsIn(m).collectFirst {
          case JsonbArrayComposite => true
        }.getOrElse(false)
      }.toSet

    def entityJsonOrJsonbArrayComposites: Set[Composite] =
      entityDataProperties.map(_.tpe).collect { case m: Composite => m }.filter { m =>
        Sql.keywordsIn(m).collectFirst {
          case JsonArrayComposite | JsonbArrayComposite => true
        }.getOrElse(false)
      }.toSet

    def idModelName = model.id.tpe.modelName

    def dataModelName = s"${model.modelName}Data"

    def refModelName = s"${model.modelName}Ref"

    def referenceType = refModelName

    def hasReferenceModel = entityRefProperties.nonEmpty

    def hasData = entityDataProperties.nonEmpty

    def hasParent = entityRefProperties.exists(_.parent)

    def parentIdProperty = entityRefProperties.find(_.parent).get

    lazy val directReferenceModel: Composite = entityRefProperties.toList match {
      case Nil      => sys.error(s"No RefProperty in ${model.modelName}")
      case xs       =>
        composite(s"${model.modelName}Reference")(entityRefProperties: _*) <+ model.features
    }

    lazy val referenceModel: Composite = entityRefProperties.toList match {
      case Nil      =>
        sys.error(s"No RefProperty in ${model.modelName}")
      case xs       =>
        composite(s"${model.modelName}Ref")(entityRefProperties:_*) <+ model.features
    }

    lazy val dataModel: Composite = composite(dataModelName)(entityDataProperties:_*) <+ model.features

    def createParameter: String =  (model.hasReferenceModel, model.hasData) match {
      case (true, true) => s"${model.modelName.camelCase}.ref, ${model.modelName.camelCase}.data"
      case (true, false) => s"${model.modelName.camelCase}.ref"
      case (false, true) => s"${model.modelName.camelCase}.data"
      case (false, false) => ""
    }

    def generateInstance: String = {
      s"""
         |${model.modelName}(
         |  ${model.id.generateNameValue}  ${if (model.hasReferenceModel || model.hasData) ", " else ""}
         |  ${if (model.hasReferenceModel) s"ref = ${model.referenceModel.generateInstance}" else ""} ${if (model.hasReferenceModel && model.hasData) ", " else ""}
         |  ${if (model.hasData) s"data = $generateDataInstance" else ""}
         |)
       """.stripMargin
    }

    def generateDataInstance: String =
      s"""
         |  $dataModelName(
         |    ${entityDataProperties.map(_.generateNameValue).mkString(",\n")}
         |  )
       """.stripMargin

    def generateTestDataInstanceFactoryMethod: String = {

      def generateTestProperty(p: Property) = p match {
        case rp: RefProperty if rp.optional =>
          s"${p.propertyName} = Some(${p.propertyName})"
        case rp: RefProperty =>
          s"${p.propertyName} = ${p.propertyName}"
        case _ =>
          p.generateNameValue
      }



      s"""
         |def create$dataModelName: $dataModelName = {
         |  $dataModelName(
         |    ${entityDataProperties.map(generateTestProperty).mkString(",\n")}
         |  )
         |}
       """.stripMargin
    }

    def generateBeforeDeleteMethod(candidateChildren: Seq[Entity]): String = {
      def generateDelete(child: Entity, rf: RefProperty): String = {
        val table = s"${child.modelName.camelCase}Table"
        s"""|$table.filter(_.${rf.propertyName} === id).map(_.id).result.flatMap { seq =>
            |  DBIO.seq(seq.map(cid => beforeDelete${child.modelName}(cid)): _*)
            |} >> $table.filter(_.${rf.propertyName} === id).delete""".stripMargin
      }
      //println(s"Models referencing ${model.modelName}: ${entityChildren(candidateChildren).map(_.modelName)} ")
      entityChildren(candidateChildren).toList match {
        case Nil =>
          s"def beforeDelete${model.modelName}(id: ${model.idModelName}) = DBIO.successful(0)"
        case children =>
          val deletes: List[String] = children.flatMap { child =>
            child.entityRefProperties.filter(p => p.tpe.entity == model)
              .map(p => generateDelete(child, p))
          }
          s"""
             |def beforeDelete${model.modelName}(id: ${model.idModelName}) =
             |  ${deletes.mkString(" >>\n")}
           """.stripMargin
      }
    }

    /*def metaModel = {

      def compositePropertyMetaModel(property: CompositeProperty): String = {
        val composite = property.tpe
        val properties = composite.properties.sqlProperties(model).map {
          case p: Property => s"""val ${p.propertyName} = s"$$column ->> '${p.propertyName}'""""
        }
        val all = composite.properties.sqlProperties(model).map {
          case p: CompositeProperty => "$" + s"{${p.propertyName}.*}"
          case p => "$" + p.propertyName
        }
        s"""
           |object ${property.propertyName} {
           |  val column = "${property.sqlName(model)}"
           |  ${properties.mkString("\n")}
           |  val * = s"${all.mkString(",")}"
           |}
       """.stripMargin
      }

      def metaProperties: Seq[String] = {
        entitySqlProperties.map {
          case p: CompositeProperty =>
            compositePropertyMetaModel(p)
          case p =>
            s"""val ${p.propertyName} = "${p.sqlName(model)}""""
        }
      }

      val all = model.entitySqlProperties.map {
        case p: CompositeProperty => "$" + s"{${p.propertyName}.*}"
        case p => "$" + p.propertyName
      }
      s"""
         |object ${model.metaName} {
         |  val table = "${model.sqlName}"
         |  ${metaProperties.mkString("\n")}
         |  val * = s"${all.mkString(",")}"
         |}
       """.stripMargin
    }*/


  }

  implicit class RouteEntityEx(model: Entity) {
    def routeSupport: String =
      if (model.hasReferenceModel)
        "SecureRouteSupport"
      else
        "SecureRouteSupport"
  }

  implicit class SqlEntityEx(model: Entity) {

    def sqlRefWriter(instanceName: String = "instance"): String = {
      model.entityRefProperties.map(p => s"$instanceName.ref.${p.propertyName}").mkString(",\n")
    }
    def sqlWriter(instanceName: String = "instance"): String = {
      def flattened(prefix: String, properties: Seq[Property]): Seq[String] =
        properties.map {
          case p: CompositeProperty if p.sqlFlatten =>
            p.tpe.properties.map {
              case cp: CompositeProperty if cp.sqlFlatten =>
                flattened(s"$prefix.${p.propertyName}.${cp.propertyName}", cp.tpe.properties).mkString(",\n")
              case nonComposite =>
                s"$prefix.${p.propertyName}.${nonComposite.sqlFlattenName}"
            }.mkString(",\n")
            //p.tpe.properties.map(inner => s"$prefix.${p.propertyName}.${inner.sqlFlattenName}")
          case p: JsondProperty =>
            s"$postgresqlJsonMappedType($prefix.${p.propertyName})"
          case p: JsonbProperty =>
            s"$postgresqlJsonbMappedType($prefix.${p.propertyName})"
          case p =>
            s"$prefix.${p.propertyName}"
        }

      val refWriter =
        if (model.hasReferenceModel)
          s"${sqlRefWriter(instanceName)},"
        else
          ""

      s"""|${model.modelName}Record(
          |  $instanceName.id,
          |  $refWriter
          |  ${flattened(instanceName + ".data", model.entityDataProperties).mkString(",\n")},
          |  $instanceName.timestamp.createdAt,
          |  $instanceName.timestamp.updatedAt)
       """.stripMargin
    }

    def sqlRefReader(recordName: String = "record"): String = {
      val ps = model.entityRefProperties.map(p => s"$recordName.${p.propertyName}").mkString(",")
      s"${model.refModelName}($ps)"
    }

    def sqlReader(recordName: String = "record"): String = {

      def unflattened(prefix: String, properties: Seq[Property]): Seq[String] =
        properties.flatMap {
          case p: CompositeProperty if p.sqlFlatten =>
            val cps =
              p.tpe.properties.map {
                case cp: CompositeProperty if p.sqlFlatten =>
                  val cpProperties = unflattened(prefix, cp.tpe.properties).mkString(", ")
                  s"${cp.tpe.modelName}($cpProperties)"
                case nonComposite =>
                  val inner = (nonComposite <+ p.features <+ model.features)
                  s"$prefix.${inner.sqlFlattenName}"
              }.mkString(", ")
            Seq(s"${p.tpe.modelName}($cps)")
          case p: JsondProperty =>
            Seq(s"$prefix.${p.propertyName}.value")
          case p: JsonbProperty =>
            Seq(s"$prefix.${p.propertyName}.value")
          case p =>
            Seq(s"$prefix.${p.propertyName}")
        }

      val refReader =
        if (model.hasReferenceModel)
          s"${sqlRefReader(recordName)},"
        else
          ""

      s"""|${model.modelName}(
          |  $recordName.id,
          |  $refReader
          |  ${model.dataModelName}(${unflattened(recordName, model.entityDataProperties).mkString(",\n")}),
          |  Timestamp($recordName.createdAt, $recordName.updatedAt))
       """.stripMargin
    }

    def sqlFeatures: String = {

      def foreignKeys: Seq[String] = {
        model.properties.zipWithIndex.collect {
          case (rp: RefProperty, i) =>
            val ip = rp.reference
            val tpe = ip.tpe
            val child = model.sqlName.snakeCase
            val childp = rp.sqlName(model).snakeCase
            val parent = tpe.entity.sqlName.snakeCase
            val parentp = ip.sqlName(tpe.entity).snakeCase
            val fkName = s"${child}_${childp}_ref_${parent}_$parentp".qoute
            s"sql.foreignKey(${fkName})(ref.${rp.propertyName})(${tpe.entity.modelName}Model, ${tpe.entity.modelName}Model.id)"
        }
      }

      def primaryKey: String = {
          val mn = model.modelName.snakeCase
          val mid = model.id.propertyName.snakeCase
          val pkName = s"${model.modelName.snakeCase}_pk".qoute
          s"sql.primaryKey(${pkName})(${model.modelName}Model.${model.id.propertyName})"
      }

      def indexes: Seq[String] = {
        Sql.keywordsIn(model).collect {
          case idx@Index(_, _) => idx
        }.zipWithIndex.map { t =>
          /*val (idx, i) = t
          val ps = model.entitySqlProperties.filter(p => idx.picker(p.sqlName(model)))
          val prefix = model.modelName.toLowerCase
          val postfix = if (idx.unique) "uidx" else "idx"
          val name = s"sql.index(${prefix}_${postfix})"
          val on = ps.toList match {
            case x :: Nil =>
              x.sqlFlattenName
            case xs       =>
              s"(${xs.map(_.sqlFlattenName).mkString(", ")})"
          }
          val unique = idx.unique
          s"def idx$i = index($name, $on, unique = $unique)"
          */
          "todo"
        }
      }

      def basic = List("sql.schema(schema)", s"sql.name(${model.modelName.snakeCase.qoute})", primaryKey)

      (basic ++ foreignKeys).mkString(",\n").stripMargin
    }
  }


}

object Models extends Models {
  val postgresqlJsonType = "json"
  val postgresqlJsonbType = "jsonb"
  val postgresqlJsonMappedType = "PgJson"
  val postgresqlJsonbMappedType = "PgJsonb"
}
