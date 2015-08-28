delete from lampetia.security_acl;
delete from lampetia.security_group_member;
delete from lampetia.security_group;
delete from lampetia.security_user;

insert into lampetia.security_user(id, account_state) values ('u-1', 'ACTIVE');
insert into lampetia.security_user(id, account_state) values ('u-2', 'ACTIVE');
insert into lampetia.security_user(id, account_state) values ('u-3', 'ACTIVE');
insert into lampetia.security_user(id, account_state) values ('u-4', 'ACTIVE');


-- group 'g-1'
insert into lampetia.security_user(id, account_state) values ('g-1-owner', 'ACTIVE');
insert into lampetia.security_group(id, owner_user_id, parent_group_id, code)
  values('g-1', 'g-1-owner', null, 'g-1');

insert into lampetia.security_group(id, owner_user_id, parent_group_id, code)
  values('g-11', 'g-1-owner', 'g-1', 'g-11');

insert into lampetia.security_group(id, owner_user_id, parent_group_id, code)
  values('g-111', 'g-1-owner', 'g-11', 'g-111');

insert into lampetia.security_group_member(group_id, member_id)
  values('g-11', 'u-1');
insert into lampetia.security_group_member(group_id, member_id)
  values('g-11', 'u-2');
insert into lampetia.security_group_member(group_id, member_id)
  values('g-11', 'u-3');
insert into lampetia.security_group_member(group_id, member_id)
  values('g-111', 'u-4');

-- group 'g-2'
insert into lampetia.security_user(id, account_state) values ('g-2-owner', 'ACTIVE');
insert into lampetia.security_group(id, owner_user_id, parent_group_id, code)
  values('g-2', 'g-2-owner', null, 'g-2');
insert into lampetia.security_group(id, owner_user_id, parent_group_id, code)
  values('g-21', 'g-2-owner', 'g-2', 'g-21');

-- list all members of group 'g-11', should return 'u-1, u-2, u-3, u-4'
select gm.group_id, gm.member_id
from   lampetia.security_group_member gm
  inner join (
    with recursive r as (
      select id, parent_group_id, code
      from   lampetia.security_group
      where  id = 'g-11'

      union all

      select g.id, g.parent_group_id, g.code
      from   lampetia.security_group g join r on g.parent_group_id = r.id
    ) select r.id as group_id from r) rec_group on gm.group_id = rec_group.group_id;

-- list all members of group 'g-111' should return only 'u-4'
select gm.group_id, gm.member_id
from   lampetia.security_group_member gm
  inner join (
    with recursive r as (
      select id, parent_group_id, code
      from   lampetia.security_group
      where  id = 'g-111'

      union all

      select g.id, g.parent_group_id, g.code
      from   lampetia.security_group g join r on g.parent_group_id = r.id
    ) select r.id as group_id from r) rec_group on gm.group_id = rec_group.group_id;

-- [ACL-1] grant user 'u-1' access to conference 'conf-1' and ANY resource under 'conf-1'
insert into lampetia.security_acl(id, subject_id, subject_type, resource_uri, permission)
  values('acl-1', 'u-1', 'SUBJECT_USER', '/conference/conf-1/.*', '00000000000000000000000000001111'::bit(32));

-- [ACL-2] grant user 'u-2' access to conference 'conf-1' ONLY
insert into lampetia.security_acl(id, subject_id, subject_type, resource_uri, permission)
  values('acl-2', 'u-2', 'SUBJECT_USER', '/conference/conf-1/', '00000000000000000000000000001111'::bit(32));

-- 'u-1' has access to all resources under 'conf-1', should return 1 row [ACL-1]
select *
from lampetia.security_acl acl
where acl.subject_id = 'u-1'
and   '/conference/conf-1/post/1234' ~ acl.resource_uri
and acl.permission & '00000000000000000000000000000010'::bit(32) = '00000000000000000000000000000010'::bit(32);

-- 'u-1' does NOT have access to 'conf-12', shouldn't return any rows
select *
from lampetia.security_acl acl
where acl.subject_id = 'u-1'
and   '/conference/conf-12/' ~ acl.resource_uri
and acl.permission & '00000000000000000000000000000010'::bit(32) = '00000000000000000000000000000010'::bit(32);

-- 'u-2' has access to 'conf-1', should return 1 row [ACL-2]
select *
from lampetia.security_acl acl
where acl.subject_id = 'u-2'
and   '/conference/conf-1/' ~ acl.resource_uri
and acl.permission & '00000000000000000000000000000010'::bit(32) = '00000000000000000000000000000010'::bit(32);









----------------

-- [ACL-3] grant goup 'g-1' access to conference 'conf-3' ONLY
insert into lampetia.security_acl(id, subject_id, subject_type, resource_uri, permission)
  values('acl-3', 'g-1', 'SUBJECT_GROUP', '/conference/conf-3/', '00000000000000000000000000001111'::bit(32));


-- select all groups that 'u-1' belongs to
with recursive r as (
  select id, parent_group_id, code
  from   lampetia.security_group parent, lampetia.security_group_member gm
  where  parent.id = gm.group_id and gm.member_id = 'u-1'
  union
  select g.id, g.parent_group_id, g.code
  from   lampetia.security_group g join r on r.parent_group_id = g.id
) select * from r;

-- select all ACL records for all groups that 'u-1' belongs to
select *
from lampetia.security_acl acl
inner join (
  with recursive r as (
    select id, parent_group_id, code
    from   lampetia.security_group parent, lampetia.security_group_member gm
    where  parent.id = gm.group_id and gm.member_id = 'u-1'
    union
    select g.id, g.parent_group_id, g.code
    from   lampetia.security_group g join r on r.parent_group_id = g.id
  ) select r.id as group_id from r
) g on acl.subject_type = 'SUBJECT_GROUP' and acl.subject_id = g.group_id;




-- check if 'u-1' has access to 'conf-3' [ACL-3]
-- 'u-1' is a member of group 'g-11' which is a child of group 'g-1' and
-- 'g-1' has access to 'conf-3' according to [ACL_3]
select *
from lampetia.security_acl acl
inner join (
  with recursive r as (
    select id, parent_group_id, code
    from   lampetia.security_group parent, lampetia.security_group_member gm
    where  parent.id = gm.group_id and gm.member_id = 'u-1'
    union
    select g.id, g.parent_group_id, g.code
    from   lampetia.security_group g join r on r.parent_group_id = g.id
  ) select r.id as group_id from r
) g on acl.subject_type = 'SUBJECT_GROUP' and acl.subject_id = g.group_id
-- now check the resource permission
where '/conference/conf-3/' ~ acl.resource_uri
and acl.permission & '00000000000000000000000000000010'::bit(32) = '00000000000000000000000000000010'::bit(32);
