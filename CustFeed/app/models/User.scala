package models

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

case class User(email: String, 
				name: String,
				password: String,
				country: Option[String],
				address: Option[String],
				age: Option[Int])

object User {
  
  // -- Parsers
  
  /**
   * Parse a User from a ResultSet
   */
  val simple = {
    get[String]("user.email") ~
    get[String]("user.name") ~
    get[String]("user.password") ~
    get[String]("user.country") ~
    get[String]("user.address") ~
    get[Int]("user.age") map {
      case email~name~password~country~address~age => User(email, name, password, Some(country), Some(address), Some(age))
    }
  }
  
  // -- Queries
  
  /**
   * Retrieve a User from email.
   */
  def findByEmail(email: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user where email = {email}").on(
        'email -> email
      ).as(User.simple.singleOpt)
    }
  }
  
  /**
   * Retrieve all users.
   */
  def findAll: Seq[User] = {
    DB.withConnection { implicit connection =>
      SQL("select * from user").as(User.simple *)
    }
  }
  
  /**
   * Authenticate a User.
   */
  def authenticate(email: String, password: String): Option[User] = {
    DB.withConnection { implicit connection =>
      SQL(
        """
         select * from user where 
         email = {email} and password = {password}
        """
      ).on(
        'email -> email,
        'password -> password
      ).as(User.simple.singleOpt)
    }
  }
   
  /**
   * Create a User.
   */
  def create(user: User): User = {
    DB.withConnection { implicit connection =>
      SQL(
        """
          insert into user values (
            {email}, {name}, {password}, {country}, {address}, {age}
          )
        """
      ).on(
		'email -> user.email,
		'name -> user.name,
		'password -> user.password,
		'country -> { user.country match {
			case Some(user.country) => user.country
			case None => ""}},
		'address -> { user.address match {
			case Some(user.address) => user.address
			case None => ""}},
		'age -> { user.age match {
			case Some(user.age) => user.age
			case None => 0}}
      ).executeUpdate()
      
      user
      
    }
  }

}
