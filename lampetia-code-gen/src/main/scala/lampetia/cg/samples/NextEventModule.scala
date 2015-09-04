package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Module

/**
 * @author Radwa Osama
 * @since 1:21 PM - 9/4/2015
 */
object NextEventModule extends App {

  val base = "nxt.event"

  val module = Module("nxt-event", base, "nxt-event")

  val models = Nil

  CodeGenerator.modelGenerator(module, models).generate()
}
