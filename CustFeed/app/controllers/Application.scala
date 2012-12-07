package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import play.api.mvc._

import models._
import views._

object Application extends Controller {
  
  /**
   * Displays the welcome page.
   */
  def index = Action {
    Ok(views.html.index())
  }
  
  //
  //  Authentication
  //
  
  val loginForm = Form(
    tuple(
      "email" -> text,
      "password" -> text
    ) verifying ("Invalid email or password", result => result match {
      case (email, password) => User.authenticate(email, password).isDefined
    })
  )
  
  /**
   *  Login page
   */
  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }
  
  /**
   *  Login form submission
   */
  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => Redirect(routes.Application.index).withSession("email" -> user._1)
    )
  }
  
  /**
   *  Logout and cleanup
   */
  def logout = Action {
    Redirect(routes.Application.login).withNewSession.flashing(
      "success" -> "You've been logged out"
    )
  }
  
  //
  //  Client
  //
  
  val clientForm = Form(
    tuple(
      "client_id" -> nonEmptyText,
      "contact" -> nonEmptyText, 
      "home_url" -> nonEmptyText
    )
  )
  
  def addClient = Action { implicit request =>
    clientForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.addClient(formWithErrors)),
      {case (client_id, contact, home_url) => Ok(views.html.addClientOk(client_id, contact, home_url))}
    )
  }
  
  /**
   * Adds a client with the given ID to the system.
    def addClient(clientID: String) = TODO
    def clientForm = TODO
   */
  
  /**
   * Gets the information for the given client.
   */
  def client(clientID: String) = TODO
  
  /**
   * Displays the details for the given client.
   */
  def editClient(clientID: String) = TODO
  
  /**
   * Removes the client with the given ID from the system.
   */
  def removeClient(clientID: String) = TODO
  
  /**
   * Posts a new review for the given client and product.
   */
  def postReview(clientID: String, productID: String) = Action(parse.json) { 
   request =>
    (request.body \ "review_text").asOpt[String].map { review_text => 
      
      var mylog= " clientID=" + clientID + " productID= " + productID + " \n"
      mylog += request.body
      (request.body \ "rating2").asOpt[String].map { rating2 =>
         mylog += "\nrating=" + rating2  + "\n" 

         (request.body \ "path").asOpt[String].map { path =>
             mylog += "\npath=" + path  + "\n" 
             ReviewData.addReview5(clientID, productID, review_text, rating2, path )
         }.getOrElse {
             ReviewData.addReview4(clientID, productID, review_text, rating2 )
         }
         Ok("Review received\n" + mylog + " \n")
        
      }.getOrElse {
         ReviewData.addReview(clientID, productID, review_text )
         Ok("Review received\n no rating2\n" + mylog + " \n")
      }
    }.getOrElse {
      BadRequest("Missing parameter [review_text]\n")
    }
  }
  
  def getReviewLast(clientID: String, productID: String) = Action { request =>
    val cursor2=ReviewData.getReviewsLast(clientID, productID)
    var mylog = cursor2.toString
    Ok ( "last=" +  mylog )
  }

  def getReviewCount(clientID: String, productID: String) = Action { request =>
    val cursor = ReviewData.getReviews(clientID, productID)
    var mysize= cursor.size; 
    var mylog  = "clientID=" + clientID
    mylog  += " productID=" + productID

    val cursor2=ReviewData.getReviewsLast(clientID, productID)
    mylog += "\n" + cursor2.toString()

    Ok ( mylog + "\n" + "size=" + mysize + "\n"  )
  }


  def getReviews(clientID: String, productID: String) = Action { request =>
    val cursor = ReviewData.getReviews(clientID, productID)
    
/**
 *   cursor.foreach(review => println(review))
 */
   var outString=0
   var outString2= ReviewData.get_pid()  + "\n\n"

   var outString3="debug"
//   var outString3=ReviewData.get_productID() // get function 
    cursor.foreach(
       review => { 
          outString += 1 ; 
          outString2 += outString + ": " + review.product_id ;
          outString2 += "_id= " + review._id ;
          outString2 += " client_id= " + review.client_id ;
          outString2 += "\n"; 
       } 
    )
    Ok("Get reviews request received " + outString + "\n" +  outString3 + "\n" + outString2)
  }

  def welcome = Action { request =>
    val was= ReviewData.addOneCount() 
    val ip=request.remoteAddress
    var cid= ReviewData.addOne(ip) 
    val current= ReviewData.addOneCount() 
    Ok( "was= " + was + " from=" + ip + " now=" + current + "\n"  )
  }

  def welcomeCount = Action { 
    var cid= ReviewData.addOneCount() 
    Ok( "count= " + cid )
  }


  def prod1 = Action {
    var cid= ReviewData.get_pid2() 
    Ok(views.html.client1( cid ))
  }

  def prod3 = Action {
    var cid= ReviewData.get_pid3() 
    Ok(views.html.client1( cid ))
  }


  def getClient(clientID: String) = Action { 
    var cid= ReviewData.get_pid4(clientID ) 
    Ok(views.html.clientOne( cid ))
  }
  
  /**
   * Posts a rating of a given existing review as "negative".
   */
  def postReviewRatingNegative(reviewID: String) = TODO
  
  /**
   * Posts a rating of a given existing review as "positive".
   */
  def postReviewRatingPositive(reviewID: String) = TODO
  
  /**
   * Posts a user's rating of a given existing review as "inappropriate".
   */
  def postReviewRatingInappropriate(reviewID: String) = TODO
  
  /**
   * Gets a summary report of the given type for the given client.
   */
  def getSummaryReport(clientID: String, reportType: String) = TODO
  
  /**
   * Gets a product report of the given type for the given client and product.
   */
  def getProductReport(clientID: String, productID: String, reportType: String) = TODO
  
  /**
   * Stores the given product review in the reviews DB.
   */
  def storeReview(clientID: String, productID: String, reviewText: String) = {}
}
