package lampetia.sql

import java.sql
import javax.sql.DataSource

import com.zaxxer.hikari.HikariDataSource

/**
 * @author Hossam Karim
 */


trait ConnectionSourceFactories {

  private class FromDataSource(dataSource: DataSource) extends JdbcConnectionSource {
    def connection: sql.Connection = dataSource.getConnection
    def done(connection: sql.Connection): Unit = connection.close()
    def shutdown(): Unit = ()
  }

  def connectionSource(dataSource: DataSource): JdbcConnectionSource = new FromDataSource(dataSource)

  private class HikariConnectionSourceFromDataSource(
    dataSourceClassName: String,
    serverName: String,
    portNumber: Int,
    databaseName: String,
    user: String,
    password: String,
    maximumPoolSize: Int,
    leakDetectionThreshold: Int) extends JdbcConnectionSource {

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

  private class HikariConnectionSourceFromJdbcUrl(
    jdbcUrl: String,
    user: String,
    password: String,
    maximumPoolSize: Int,
    leakDetectionThreshold: Int) extends JdbcConnectionSource {

    lazy val dataSource = {
      val ds = new HikariDataSource()
      ds.setMaximumPoolSize(maximumPoolSize)
      ds.setLeakDetectionThreshold(leakDetectionThreshold)
      ds.setJdbcUrl(jdbcUrl)
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
             leakDetectionThreshold: Int): JdbcConnectionSource =
    new HikariConnectionSourceFromDataSource(
      dataSourceClassName,
      serverName,
      portNumber,
      databaseName,
      user,
      password,
      maximumPoolSize,
      leakDetectionThreshold)


  def hikari(jdbcUrl: String,
             user: String,
             password: String,
             maximumPoolSize: Int,
             leakDetectionThreshold: Int): JdbcConnectionSource =
    new HikariConnectionSourceFromJdbcUrl(
      jdbcUrl,
      user,
      password,
      maximumPoolSize,
      leakDetectionThreshold)

}
