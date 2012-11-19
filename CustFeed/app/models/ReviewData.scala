package models

import play.api.Play.current
import java.util.{Date}
import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import salatcontext._

case class Review (
  text: Option[String] = None,
  rating: Option[String] = None
)

case class Feedback (
  positive_count: Option[Int] = None,
  negative_count: Option[Int] = None,
  inappropriate: Option[Int] = None
)

case class ReviewData(
  _id: ObjectId = new ObjectId,
  client_id: Option[String] = None, 
  product_id: Option[String] = None,
  review_date: Option[String] = None ,
  review: Option[Review] = None,
  feedback: Option[Feedback] = None,
  review_text: Option[String] = None
//  rating: Option[Int] = None,
//  reviewer_id: Option[String] = None,
//  review_date: Option[String] = None,
//  positive_count: Option[Int] = None, 
//  negative_count: Option[Int] = None, 
//  inappropriate_count: Option[Int] = None
)

object ReviewData extends ModelCompanion[ReviewData, ObjectId] {
  val collection = mongoCollection("reviews")
  val dao = new SalatDAO[ReviewData, ObjectId](collection = collection) {}
  // val rating=getReviewByClientID("client_svl")
  val rating = dao.find(MongoDBObject("client_id" -> "client_svl"  ))
  val ratingA=rating.toArray;

  val tbd = { 
      var i=0
      var myReview=ratingA(i).review
      var myFeedback=ratingA(i).feedback
      var myRA=myReview.toArray
      var myFA=myFeedback.toArray
      var first= "_id client_id product_id rating positive negative \n" 
      for ( i <- 0 to ratingA.size-1 ) {
         myReview=ratingA(i).review
         myRA=myReview.toArray
         first += ratingA(i)._id.toString + " "
         first += ratingA(i).client_id.get + " " +ratingA(i).product_id.get + " " 
         first += myRA(0).rating.get + " " + myRA(0).text.get  + " "
         first += myFA(0).positive_count.get + " " + myFA(0).negative_count.get + "\n" 
      }

      first   
   }

 
  def getReviews(clientID: String, productID: String) = dao.find(MongoDBObject("client_id" -> clientID), MongoDBObject("product_id" -> productID))

  def getReviewByClientID(clientID: String): Option[ReviewData] = dao.findOne(MongoDBObject("client_id" -> clientID  ))
  def addReview(clientID: String, productID: String, reviewText: String) = dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text = Option(reviewText)))

  def get_pid() = {  tbd } 

  var tbd1A = Array ( ratingA(0), ratingA(1) )
  def get_pid2() = { ratingA  } 

}
