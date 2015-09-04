package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.{EnableTestingFeatures, NonSecure, Module}
import lampetia.cg.extensions.Models._

/**
 * @author Hossam Karim
 */
object MusicModule extends App {

  val Genre =
    enum("Genre")("Jazz".ecase, "Flamenco".ecase, "Classic".ecase).withDiscriminator("name".string) <+
      commonFeatures <<
      (named("music_genre") in Sql)

  val Artist =
    entity("Artist")("name".name, "birthDate".dateTime, "bio".jsonb) <+
      commonFeatures <<
      (named("music_artist") in Sql)

  val Song =
    entity("Song")(
      "name".name,
      ("performer" parentRef Artist.id) /* << (named("something") in Sql) */,
      "composer" ref Artist.id,
      "arranger" optionalRef Artist.id
    ) <+ commonFeatures <<
      (named("music_song") in Sql) <<
      (index(_ == "composer") in Sql) <<
      (index(_ == "performer") in Sql) <<
      (index(_ == "arranger") in Sql)

  val SongData = Song.dataModel << (jsonbArrayComposite in Sql)

  val Album =
    entity("Album")(
      "name".name,
      "artist" optionalRef Artist.id,
      ("songs" listOf SongData) << (jsonbComposite in Sql)
    ) <+ commonFeatures << (named("music_album") in Sql)


  val module = Module("Music", "com.example.music", "music", options = Seq(EnableTestingFeatures))
  val models = Seq(
    Genre,
    Artist.id.tpe,
    Artist,
    Song.id.tpe,
    Song,
    Album.id.tpe,
    Album)

  CodeGenerator.serviceGenerator(module, models).generate()
  CodeGenerator.scriptGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.nodeGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.deployGenerator(module, models).generate(formatScalaSource = false)
  CodeGenerator.utilGenerator(module, models).generate(formatScalaSource = false)

}
