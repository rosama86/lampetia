package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */

// user -> followers
object FollowerModule2 extends App {

  val FollowedModel = Resource << (jsonbArrayComposite in Sql)

  val FollowerModel =
    entity("Follower")(
      ("followers" listOf FollowedModel) << (jsonbArrayComposite in Sql)
    ) <+ commonFeatures

  val module = Module("Follower", "lampetia.follower", "lampetia")
  val models = Seq(FollowerModel.id.tpe, FollowerModel)
  CodeGenerator.serviceGenerator(module, models).generate()
}

/*
Notes
-----

- Determine whom is $userId is following:

```
  select f.follower "user", id "is following"
  from (
    select id, jsonb_array_elements(followers) ->> 'id' as follower
    from lampetia.follower
  ) f
  where f.follower = $userId

```

- Determine who is following $userId:

```
  select id, followers
  from lampetia.follower
  where id = $userId
```


 */
