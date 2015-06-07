package lampetia.sql.syntax

import java.sql

import com.zaxxer.hikari.HikariDataSource

/**
 * @author Hossam Karim
 */


trait ConnectionSourceFactories {
  self: JdbcCodec =>

  private class Hikari(
    dataSourceClassName: String,
    serverName: String,
    portNumber: Int,
    databaseName: String,
    user: String,
    password: String,
    maximumPoolSize: Int,
    leakDetectionThreshold: Int) extends ConnectionSource {

    lazy val dataSource = {
      val ds = new HikariDataSource()
      ds.setMaximumPoolSize(maximumPoolSize)
      ds.setLeakDetectionThreshold(leakDetectionThreshold)
      //ds.setDataSourceClassName("org.postgresql.ds.PGSimpleDataSource")
      ds.setDataSourceClassName(dataSourceClassName)
      ds.addDataSourceProperty("serverName", serverName)
      ds.addDataSourceProperty("portNumber", portNumber)
      ds.addDataSourceProperty("databaseName", databaseName)
      ds.addDataSourceProperty("user", user)
      ds.addDataSourceProperty("password", password)
      ds
    }

    def connection: sql.Connection = dataSource.getConnection

    def done(connection: Connection): Unit = connection.close()

    def shutdown(): Unit = dataSource.close()

  }

  def hikari(dataSourceClassName: String,
             serverName: String,
             portNumber: Int,
             databaseName: String,
             user: String,
             password: String,
             maximumPoolSize: Int,
             leakDetectionThreshold: Int): ConnectionSource =
    new Hikari(dataSourceClassName,serverName,portNumber,databaseName,user,password,maximumPoolSize,leakDetectionThreshold)

}
