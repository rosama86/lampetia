package lampetia.metamodel

/**
 * @author Hossam Karim
 */

case class SqlFeature(keyword: Keyword) extends Feature

trait SqlScope extends FeatureScope {
  type F = SqlFeature

  def feature(keyword: Keyword): F = SqlFeature(keyword)

  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@SqlFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case SqlFeature(k) => k
    }
}

trait SqlScopeImplicits extends NamingSupport { this: SqlScope =>

  implicit val flattenEv = evidence[this.type, Flatten]
  implicit val indexEv = evidence[this.type, Index]

  implicit def jsonCompositeEv[JC <: JsonCompositeKeyword]: ScopedKeyword[this.type, JC] = evidence[this.type, JC]
}

