package controllers

import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.mvc._
import play.api.libs.json._
import models.ReviewData
import models.SysINFO
import scala.collection.mutable.ArrayBuffer
import com.mongodb.casbah.Imports._
import scala.util.parsing.json.JSONObject._


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

  
  def getSysINFO = Action { request =>
    val ret=SysINFO.getFew
    Ok(views.html.SysINFO( ret.slice( ret.size-10, ret.size ) ))
  }
  
  

  def postAccess(clientID: String, productID: String) = Action(parse.json) { 
   request =>
      var mylog= " clientID=" + clientID + " productID= " + productID + " \n"
      mylog += request.body

    (request.body \ "review_rating").asOpt[String].map { review_rating =>
        
        (request.body \ "access_from").asOpt[String].map { access_from =>
            mylog += "\nclientID=" + clientID  + "\n" 
            mylog += "\naccess_from=" + access_from  + "\n" 
            ReviewData.addAccess(clientID, productID,  access_from )
            Ok("Review received\n" + mylog + " \n") 
        }.getOrElse {
           BadRequest("Missing parameter [access_from]\n")
        }
    }.getOrElse {
      BadRequest("Missing parameter [review_rating]\n")
    }

  }







  def postReview(clientID: String, productID: String) = Action(parse.json) { 
   request =>
    (request.body \ "review_text").asOpt[String].map { review_text => 
      
      var mylog= " clientID=" + clientID + " productID= " + productID + " \n"
      mylog += request.body
      (request.body \ "review_rating").asOpt[String].map { review_rating =>
         mylog += "\nrating=" + review_rating  + "\n" 

         (request.body \ "review_rating").asOpt[String].map { review_rating =>
             mylog += "\nreview_rating=" + review_rating  + "\n" 
             ReviewData.addReview5(clientID, productID, review_text, review_rating )
         }.getOrElse {
             ReviewData.addReview4(clientID, productID, review_text, review_rating )
         }
         Ok("Review received\n" + mylog + " \n")
        
      }.getOrElse {
         ReviewData.addReview(clientID, productID, review_text )
         Ok("Review received\n no review_rating\n" + mylog + " \n")
      }
    }.getOrElse {
      BadRequest("Missing parameter [review_text]\n")
    }
  }

  
  def getReviewLast(clientID: String, productID: String) = Action { request =>
    val cursor2=ReviewData.getReviewsLast(clientID, productID)
    Ok ( views.html.ReviewLast( cursor2 ) )
  }

  def getReviewCount(clientID: String, productID: String) = Action { request =>
    val cursor = ReviewData.getReviews(clientID, productID)
    var mysize= 0; 
    if ( cursor != null ) {
        mysize= cursor.size -1 ; 
    }

    var mylog  = "clientID=" + clientID
    mylog  += " productID=" + productID + "\n"

    val cursor2=ReviewData.getReviewsSum(clientID, productID)
    var sum=""
    if ( cursor2 != null ) {
        cursor2 foreach { n =>
            sum +=  n.name.get + ":"  + n.count.get + " " 
        }
    }

    Ok ( mylog + "Total= " +   mysize + " with distrubution of " + sum +  "\n"  )
  }


  def getReviews(clientID: String, productID: String) = Action { request =>
    val cursor = ReviewData.getReviews(clientID, productID)
    
   var outString=0
   var outString2= ReviewData.get_pid()  + "\n\n"

   var outString3="debug"
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

  def prod3 = Action {
    var cid= ReviewData.get_pid3() 
    Ok(views.html.client1( cid ))
  }


  def getClient(ClientID: String) = Action { 
    var cid= ReviewData.ClientSummary(ClientID ) 

    var log=""

    Ok(views.html.clientOne(cid , log    ))


  }
  
  def client(clientID: String) = TODO
  def editClient(clientID: String) = TODO
  def removeClient(clientID: String) = TODO
  def postReviewRatingNegative(reviewID: String) = TODO
  def postReviewRatingPositive(reviewID: String) = TODO
  def postReviewRatingInappropriate(reviewID: String) = TODO
  def getSummaryReport(clientID: String, reportType: String) = TODO
  def getProductReport(clientID: String, productID: String, reportType: String) = TODO
  def storeReview(clientID: String, productID: String, reviewText: String) = {}
}
