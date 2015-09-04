package lampetia.metamodel

/**
 * @author Hossam Karim
 */

case class ModelFeature(keyword: Keyword) extends Feature
case class ServiceFeature(keyword: Keyword) extends Feature
case class TemplateFeature(keyword: Keyword) extends Feature



trait ModelScope extends FeatureScope {
  type F = ModelFeature

  def feature(keyword: Keyword): F = ModelFeature(keyword)

  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@ModelFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case ModelFeature(k) => k
    }
}

trait ServiceScope extends FeatureScope {
  type F = ServiceFeature

  def feature(keyword: Keyword): F = ServiceFeature(keyword)

  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@ServiceFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case ServiceFeature(k) => k
    }
}

trait TemplateScope extends FeatureScope {
  type F = TemplateFeature

  def feature(keyword: Keyword): F = TemplateFeature(keyword)

  def features(fo: FeatureOwner): Seq[Feature] =
    fo.features.collect { case f@TemplateFeature(_) => f}

  def keywordsIn[FO <: FeatureOwner](fo: FO): Seq[Keyword] =
    fo.features.collect {
      case TemplateFeature(k) => k
    }
}

trait ModelScopeImplicits extends NamingSupport { this: ModelScope =>
  implicit val dependsOnEv = evidence[this.type, DependsOn]
}

trait ServiceScopeImplicits extends NamingSupport { this: ServiceScope =>
  implicit val dependsOnEv = evidence[this.type, DependsOn]
}

trait TemplateScopeImplicits extends NamingSupport { this: TemplateScope =>
  implicit val importsEv = evidence[this.type, Imports]
}

