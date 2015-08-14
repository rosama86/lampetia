package lampetia.meta.feature.sql

import lampetia.meta.PropertyType

/**
 * @author Hossam Karim
 */


trait SqlTypes {
  def name(propertyType: PropertyType[_]): String
}
