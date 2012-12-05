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
  rating2: Option[String] = None,
  path: Option[String] = None,
  access_from: Option[String] = None,
  access_date: Option[String] = None,
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
  val rating3 = dao.find(MongoDBObject("client_id" -> "spider"  ))
  val rating3A=rating3.toArray;

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

 
  def getReviews(clientID: String, productID: String) = dao.find(MongoDBObject("client_id" -> clientID, "product_id" -> productID))
  def getReviewsLast(clientID: String, productID: String) =  {
       var cursor = dao.find(MongoDBObject("client_id" -> clientID, "product_id" -> productID))
       var cursorA= cursor.toArray 
       val mysize=cursorA.size
       var lastOne=cursorA(mysize-1 )
       
       lastOne   
  }

  def getReviewByClientID(clientID: String): Option[ReviewData] = dao.findOne(MongoDBObject("client_id" -> clientID  ))
  def addReview(clientID: String, productID: String, review_text: String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt)  )) 
  }

  def addReview4(clientID: String, productID: String, review_text: String , rating2:String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt), rating2=Option(rating2)  )) 
  }

  def addReview5(clientID: String, productID: String, review_text: String , rating2:String, path:String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt), rating2=Option(rating2), path=Option(path)  )) 
  }

  def get_pid() = {  tbd } 

  def get_pid2() = { ratingA  } 
  def get_pid3() = { rating3A  } 
  def get_pid4(ClientID:String ) = { 
     val condObject= MongoDBObject ( "path"->1, "client_id"->1, "product_id"->1 , "rating2"->1 ); 
     val tObject= MongoDBObject ( "$exists" ->true ); 

     val rating4 = dao.find(MongoDBObject("client_id" -> ClientID, "path"->tObject  ), condObject )
     val ratingA=rating4.toArray;
     ratingA
 } 

  def addOne( ip:String ) = {   
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( access_date=Option(cur_txt), 
        access_from=Option(ip)  )) 
   } 

  def addOneCount( ) = {   
     val condObject= MongoDBObject ( "access_from"->1, "access_date"->1  ); 
     val tObject= MongoDBObject ( "$exists" ->true ); 
     val rating4 = dao.find(MongoDBObject( "access_from"->tObject  ), condObject )
     val ratingA=rating4.toArray;
     ratingA.size

   } 

}
