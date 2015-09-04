package lampetia.cg.samples

import lampetia.cg.CodeGenerator
import lampetia.metamodel.Dsl._
import lampetia.metamodel.Module

/**
 * @author Hossam Karim
 */

// user -> following
object FollowerModule extends App {

  val FollowedModel = Resource << (jsonbArrayComposite in Sql)

  val FollowerModel =
    entity("Follower")(
      ("following" listOf FollowedModel) << (jsonbArrayComposite in Sql)
    ) <+ commonFeatures

  val module = Module("Follower", "lampetia.follower", "lampetia")
  val models = Seq(FollowerModel.id.tpe, FollowerModel)
  CodeGenerator.serviceGenerator(module, models).generate()
}

/*
Notes
-----

- Determine whom user $userId is following:

```
  select jsonb_array_elements(following) -> 'id' as following
  from lampetia.follower
  where id = $userId;

```

- Determine who is following $userId:

```
  select id, following
  from (
    select id, jsonb_array_elements(following) ->> 'id' as following
    from lampetia.follower
  ) f
  where f.following = $userId;
```

Examples
--------
```
select * from lampetia.follower;

select id "user", jsonb_array_elements(following) -> 'id' as "is following"
from lampetia.follower
where id = '50bafdff-2d12-4efb-89b4-99c4cb7a8ef6';

select following "user", id "is followed by (followers)"
  from (
    select id, jsonb_array_elements(following) ->> 'id' as following
    from lampetia.follower
  ) f
where f.following = 'DIhpdmM5v7'
```
 */