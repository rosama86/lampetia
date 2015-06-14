package lampetia.model.sql

import lampetia.model.PropertyType

/**
 * @author Hossam Karim
 */


trait SqlTypes {
  def name(propertyType: PropertyType[_]): String
}
