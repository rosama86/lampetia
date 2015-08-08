package lampetia.security.util

/**
 * @author Hossam Karim
 */

import lampetia.security.module.SecurityModule._
import lampetia.sql.dialect.postgres.jdbc._

trait DDL {

  def ddl =
    s"create schema $schema".sql +: (
      UserModel.createSql ++
      ProfileModel.createSql ++
      RoleModel.createSql ++
      GroupModel.createSql ++
      GroupMemberModel.createSql ++
      AclModel.createSql ++
      AclRoleModel.createSql
    )

}

object DDLPrinter extends DDL {

  def main(args: Array[String]): Unit = {
    ddl.map(sql => s"${sql.sqlString};").foreach(println)
  }

}
