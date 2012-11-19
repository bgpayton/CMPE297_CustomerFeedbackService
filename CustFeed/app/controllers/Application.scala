package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import models.ReviewData

object Application extends Controller {

  val clientForm = Form(
    tuple(
      "client_id" -> nonEmptyText,
      "contact" -> nonEmptyText, 
      "home_url" -> nonEmptyText
    )
  )
  
  def index = Action {
    Ok(views.html.index())
  }

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
  def postReview(clientID: String, productID: String) = Action(parse.json) { request =>
    (request.body \ "review_text").asOpt[String].map { review_text => 
      ReviewData.addReview(clientID, productID, review_text)
      Ok("Review received")
    }.getOrElse {
      BadRequest("Missing parameter [review_text]")
    }
  }
  
  /**
   * Gets all reviews for the given client and product.
   */
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

  def prod1 = Action {

    var cid= ReviewData.get_pid2() 
    Ok(views.html.client1( cid ))
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
