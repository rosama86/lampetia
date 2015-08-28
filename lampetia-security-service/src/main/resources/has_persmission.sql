-- returns the number of records that prove `p_subject_id` has permission `p_mask` on a resource with id `p_resource_uri`
create or replace function lampetia.has_permission(p_subject_id text, p_resource_uri text, p_mask bit) returns integer as
$$
    -- noinspection SqlDialectInspection
    SELECT 1
    FROM lampetia.security_acl ssg
    INNER JOIN ( WITH RECURSIVE r AS
                  ( SELECT id,
                           resource_uri
                   FROM lampetia.security_acl acl
                   WHERE acl.subject_id = p_subject_id
                     AND acl.resource_uri ~ p_resource_uri)
                SELECT id
                FROM r ) ssgr ON ssg.id = ssgr.id
    LEFT OUTER JOIN lampetia.security_acl_role sgr ON ssg.id = sgr.acl_id
    LEFT OUTER JOIN lampetia.security_role sr ON sgr.role_id = sr.id
    WHERE (ssg.permission | coalesce(sr.permission, 0::bit(32))) & (p_mask::bit(32)) = p_mask::bit(32)
    UNION
    SELECT 1
    FROM lampetia.security_acl acl
    INNER JOIN ( WITH RECURSIVE r AS
                  ( SELECT id,
                           parent_group_id,
                           code
                   FROM lampetia.security_group parent,
                        lampetia.security_group_member gm
                   WHERE parent.id = gm.group_id
                     AND gm.member_id = p_subject_id
                   UNION SELECT g.id,
                                g.parent_group_id,
                                g.code
                   FROM lampetia.security_group g
                   JOIN r ON r.parent_group_id = g.id )
                SELECT r.id AS group_id
                FROM r ) g ON acl.subject_type = 'SUBJECT_GROUP'
    AND acl.subject_id = g.group_id
    AND acl.resource_uri ~ p_resource_uri
    LEFT OUTER JOIN lampetia.security_acl_role sgr ON (acl.id = sgr.acl_id)
    LEFT OUTER JOIN lampetia.security_role sr ON (sr.id = sgr.role_id)
    WHERE (acl.permission | coalesce(sr.permission, 0::bit(32))) & (p_mask::bit(32)) = p_mask::bit(32)

$$ language sql;


