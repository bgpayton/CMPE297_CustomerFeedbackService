package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._

import anorm._
import views._
import models._

object SignUp extends Controller {
  
	/**
	 *  Sign Up Form definition
	 */
	val signupForm: Form[User] = Form(
		
		// Define a mapping for data
		mapping(
			"email" -> email,
			"name" -> text(minLength = 4),
			
			// Create a tuple for pw
			"password" -> tuple(
				"main" -> text(minLength = 6),
				"confirm" -> text
			).verifying(
				"Passwords don't match", passwords => passwords._1 == passwords._2
			),
			
			"country" -> optional(text),
			"address" -> optional(text),
			"template" -> optional(text),
			"age" -> optional(number(min = 18, max = 100)),
			"accept" -> checked("You must accept the conditions")
		)
		
		// Binding: Create a User from the mapping result
		{
			(email, name, passwords, country, address, template, age, _) => 
			User(NotAssigned, email, name, passwords._1, country, address, template, age) 
		} 
		{
			// Unbinding: Create the mapping values from an existing User value
			user => Some(user.email, user.name, (user.password, ""), user.country, user.address, user.template, user.age, false)
		}.verifying(
			// Add an additional constraint: The name must not be taken (you could do an SQL request here)
			"This name is not available",
			user => !Seq("admin", "guest").contains(user.name)
		)
	)
	
	/**
	 *  Display form
	 */
	def form = Action {
		Ok(html.signup(signupForm));
	}
	
	/**
	 *  Display test
	 */
	def test = Action {
		val existingUser = User(NotAssigned, "fake@gmail.com", "fakeuser", "secret", Some("France"), Some("555 Bailey Ave"), Some("Yes/No"), Some(30))
		Ok(html.signup(signupForm.fill(existingUser)))
	}
	
	/**
	 *  Display user
	 */
	def user(emailID: String) = Action { implicit request =>
          val u = User.findByEmail(emailID)
          val existingUser = u.getOrElse(User(NotAssigned, "fake@gmail.com", "fakeuser", "secret", Some("France"), Some("555 Bailey Ave"), Some("Yes/No"), Some(30)))
          Ok(html.signupSummary(existingUser))
	}
	
	/**
	 *  Update user
	 */
	def update(emailID: String) = Action { implicit request =>
          val u = User.findByEmail(emailID)
          val existingUser = u.getOrElse(User(NotAssigned, "fake@gmail.com", "fakeuser", "secret", Some("France"), Some("555 Bailey Ave"), Some("Yes/No"), Some(30)))
          Ok(html.signup(signupForm.fill(existingUser)))
	}
	
	/**
	 *  Process form
	 */
	def submit = Action { implicit request =>
		signupForm.bindFromRequest.fold(
			errors => BadRequest(html.signup(errors)),
			{
				user => User.create(user)
				Ok(html.signupSummary(user))
			}
		)
	}
}
