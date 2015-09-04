package lampetia.metamodel

/**
 * @author Hossam Karim
 */

trait NamingSupport extends ScopedKeywordEvidenceBuilder { this: FeatureScope =>

  implicit val nameEv = evidence[this.type, NameKeyword]

  implicit def nameFormatEv[NF <: NameFormatKeyword]: ScopedKeyword[this.type, NF] = evidence[this.type, NF]

}
