package lampetia.metamodel

/**
 * @author Hossam Karim
 */

case class JsonFeature(keyword: Keyword) extends Feature
trait JsonScope extends FeatureScope {
  type F = JsonFeature

  def feature(keyword: Keyword): F = JsonFeature(keyword)

  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@JsonFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case JsonFeature(k) => k
    }
}

case class JsonReaderFeature(keyword: Keyword) extends Feature
trait JsonReaderScope extends FeatureScope {
  type F = JsonReaderFeature
  def feature(keyword: Keyword): F = JsonReaderFeature(keyword)
  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@JsonReaderFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case JsonReaderFeature(k) => k
    }
}

case class JsonWriterFeature(keyword: Keyword) extends Feature
trait JsonWriterScope extends FeatureScope {
  type F = JsonWriterFeature
  def feature(keyword: Keyword): F = JsonWriterFeature(keyword)
  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@JsonWriterFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case JsonWriterFeature(k) => k
    }
}

trait JsonScopeImplicits extends NamingSupport { this: JsonScope =>

}

trait JsonReaderScopeImplicits extends NamingSupport{ this: JsonReaderScope =>

}

trait JsonWriterScopeImplicits extends NamingSupport{ this: JsonWriterScope =>

}


