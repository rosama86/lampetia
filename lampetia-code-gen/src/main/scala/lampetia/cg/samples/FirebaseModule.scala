package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Module
import lampetia.metamodel.Dsl._
import lampetia.cg.extensions.Models._

/**
 * @author Hossam Karim
 */


object FirebaseModule extends App {

  val base = "com.firebase"

  val module = Module("Firebase", base, "firebase")

  val json = entity("JsonObject")("source".jsonb) <+ (commonFeatures)

  val addJsonObjectRequest = composite("AddJsonObjectRequest")("retries".int, "value" of json.dataModel)

  val addJsonObjectResponse = composite("AddJsonObjectResponse")("value" of idToValue(json.id.tpe))

  val models = Seq(json.id.tpe, json, addJsonObjectRequest, addJsonObjectResponse)

  //CodeGenerator.scriptGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.serviceGenerator(module, models).generate()


}
