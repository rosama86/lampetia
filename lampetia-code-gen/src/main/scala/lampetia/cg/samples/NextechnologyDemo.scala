package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{BaseGenerationDirectory, NonSecure, StringLiteral, Module}

/**
 * @author Hossam Karim
 */
object NextechnologyDemo extends App {

  val base = "nt"

  val moduleOptions = Seq(NonSecure, BaseGenerationDirectory("/Users/hossamkarim/tmp/delme"))

  val module = Module("NtDemo", base, "nt", options = moduleOptions)

  val Organization = entity("Organization")(
    "name".name,
    "url".string,
    "logoUrl".string,
    "about".string,
    "views".int,
    "comments".int,
    "likes".int) <+ commonFeatures << (camelCase in Json)

  val Venue = entity("Venue")(
    "name".name,
    "address".string,
    "url".string,
    "imageUrl".string,
    "email".email,
    "views".int,
    "comments".int,
    "likes".int) <+ commonFeatures << (camelCase in Json)

  val Conference = entity("Conference")(
    "organizer" ref Organization.id,
    "venue" ref Venue.id,
    "name".name,
    "starts".dateTime,
    "ends".dateTime,
    "imageUrl".string,
    "views".int,
    "comments".int,
    "likes".int) <+ commonFeatures << (camelCase in Json)


  val ConferenceSponsor = entity("ConferenceSponsor")(
    "conference" ref Conference.id,
    "organization" ref Organization.id,
    "level".string) <+ commonFeatures << (camelCase in Json)

  val Speaker = entity("Speaker")(
    "name".name,
    "title".string,
    "about".string,
    "imageUrl".string) <+ commonFeatures << (camelCase in Json)

  val Talk = entity("Talk")(
    "conference" ref Conference.id,
    "speaker" ref Speaker.id,
    "title".string,
    "summary".string,
    "headerImage".string,
    "avatarImage".string,
    "videoUrl" optionOf StringLiteral,
    "views".int,
    "comments".int,
    "likes".int,
    "room".string,
    "starts".dateTime,
    "ends".dateTime) <+ commonFeatures << (camelCase in Json)

  val TalkComment = entity("TalkComment")(
    "talk" ref Talk.id,
    "commenter".email,
    "comment".string) <+ commonFeatures << (camelCase in Json)

  val models = Seq(
    Organization.id.tpe,
    Organization,
    Venue.id.tpe,
    Venue,
    Conference.id.tpe,
    Conference,
    ConferenceSponsor.id.tpe,
    ConferenceSponsor,
    Speaker.id.tpe,
    Speaker,
    Talk.id.tpe,
    Talk,
    TalkComment.id.tpe,
    TalkComment)

  CodeGenerator.serviceGenerator(module, models).generate()
  CodeGenerator.nodeGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.deployGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.utilGenerator(module, models).generate(formatScalaSource = false)

}
