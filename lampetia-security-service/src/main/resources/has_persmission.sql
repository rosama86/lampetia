-- returns the number of records that prove `p_subject_id` has permission `p_mask` on a resource with id `p_resource_id`
create or replace function lampetia.has_permission(p_subject_id text, p_resource_id text, p_mask bit(32)) returns integer as
$$
  select 1
  from
         lampetia.security_acl ssg
           inner join (
             with recursive r as (
                 select id, resource_id, parent_resource_id
                 from   lampetia.security_acl
                 where  subject_id    = p_subject_id
                 and    resource_id   = p_resource_id

                 union all

                 select p.id, p.resource_id, p.parent_resource_id
                 from   lampetia.security_acl p join r on r.parent_resource_id = p.resource_id
            ) select id from r
           ) ssgr on ssg.id = ssgr.id
           left outer join lampetia.security_acl_role sgr on ssg.id = sgr.acl_id
           left outer join lampetia.security_role sr on sgr.role_id = sr.id
  where  (ssg.permission | coalesce(sr.permission, 0::bit(32))) & (p_mask::bit(32)) = p_mask::bit(32)

  union

  select 1
  from
         lampetia.security_acl ssg
           inner join (
             with recursive r as (
                 select id, resource_id, parent_resource_id
                 from   lampetia.security_acl
                 where  resource_id = p_resource_id

                 union all

                 select p.id, p.resource_id, p.parent_resource_id
                 from   lampetia.security_acl p join r on r.parent_resource_id = p.resource_id
            ) select id from r
           ) ssgr on ssg.id = ssgr.id
           inner join lampetia.security_group_member sgm
             on (ssg.subject_id = sgm.group_id and sgm.member_id = p_subject_id)
           left outer join lampetia.security_acl_role sgr
             on (ssg.id = sgr.acl_id)
           left outer join lampetia.security_role sr
             on (sr.id = sgr.role_id)
  where  (ssg.permission | coalesce(sr.permission, 0::bit(32))) & (p_mask::bit(32)) = p_mask::bit(32)

$$ language sql;


