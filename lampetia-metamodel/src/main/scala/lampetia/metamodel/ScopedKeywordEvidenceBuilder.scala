package lampetia.metamodel

/**
 * @author Hossam Karim
 */

trait ScopedKeywordEvidenceBuilder {

  protected[this] def evidence[FS <: FeatureScope, K <: Keyword] =
    new ScopedKeyword[FS, K] {
      def feature(scope: FS, keyword: Keyword): FS#F =
        scope.feature(keyword)
    }

}
