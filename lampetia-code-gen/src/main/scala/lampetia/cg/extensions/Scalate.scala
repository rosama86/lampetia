package lampetia.cg.extensions

/**
 * @author Hossam Karim
 */

object Scalate {

  def seperate[A](iterable: Seq[A], seperator: String = ",")(f: A => String) =
    iterable.map(f).mkString(seperator)

}
