package models

import play.api.Play.current
import java.util.{Date}
import com.novus.salat._
import com.novus.salat.dao._
import com.mongodb.casbah.Imports._
import se.radley.plugin.salat._
import salatcontext._
import java.text.SimpleDateFormat
import scala.collection.mutable.HashSet
import scala.util.parsing.json.JSON._


case class NameCount (
  name: Option[String] = None,
  count: Option[Int]=None
)

case class ProductReport (
  name: Option[String] = None,
  rating: List[NameCount] = Nil,
  total: Option[Int]=None
)

case class ReviewData(
  _id: ObjectId = new ObjectId,
  client_id: Option[String] = None, 
  product_id: Option[String] = None,
  review_rating: Option[String] = None,
  access_date: Option[String] = None,
  access_from: Option[String] = None,
  review_date: Option[String] = None ,
  review_text: Option[String] = None


)

object ReviewData extends ModelCompanion[ReviewData, ObjectId] {
  val collection = mongoCollection("reviews")
  val dao = new SalatDAO[ReviewData, ObjectId](collection = collection) {}
  // val rating=getReviewByClientID("client_svl")
  val rating = dao.find(MongoDBObject("client_id" -> "client_svl"  ))
  val ratingA=rating.toArray;
  val rating3 = dao.find(MongoDBObject("client_id" -> "spider"  ))
  val rating3A=rating3.toArray;

 
  def getReviews(clientID: String, productID: String) = {
      dao.find(MongoDBObject("client_id" -> clientID, "product_id" -> productID ) )

  }

  def getReviewsLast(clientID: String, productID: String) =  {
       var cursor = dao.find(MongoDBObject("client_id" -> clientID, "product_id" -> productID))
       var cursorA= cursor.toArray 
       
       cursorA.slice( cursorA.size-1, cursorA.size  ) 
  }

  def getReviewByClientID(clientID: String): Option[ReviewData] = dao.findOne(MongoDBObject("client_id" -> clientID  ))
  def addReview(clientID: String, productID: String, review_text: String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt)  )) 
  }

  def addReview4(clientID: String, productID: String, review_text: String , review_rating:String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt), review_rating=Option(review_rating)  )) 
  }

  def addReview5(clientID: String, productID: String, review_text: String , review_rating:String ) = {
     val current=new Date();
     val cur_txt=current.toString(); 
     dao.insert( new ReviewData( client_id = Option(clientID), product_id = Option(productID), review_text=Option(review_text), review_date=Option(cur_txt), review_rating=Option(review_rating)  )) 
  }

  def addAccess(clientID: String, productID: String, accessFrom:String ) = {
     val current=new Date();
     val formater=new SimpleDateFormat("yyyy/mm/dd") 
     val mystring= formater.format(current)

     dao.insert( new ReviewData( client_id = Option(clientID), 
         product_id = Option(productID), 
         access_date=Option(mystring), 
         access_from=Option(accessFrom) 
           )) 
  }

  def get_pid() = {  ratingA } 

  def get_pid2() = { ratingA  } 
  def get_pid3() = { rating3A  } 
  def ClientSummary(ClientID:String ) = { 
     val condObject= MongoDBObject (  "client_id"->1, "product_id"->1 , "review_rating"->1 ); 
     val tObject= MongoDBObject ( "$exists" ->true ); 

     val rating4 = dao.find(MongoDBObject("client_id" -> ClientID   ), condObject )
     val ratingA=rating4.toArray;

     val map=scala.collection.mutable.HashMap.empty[String, Int ]
     val map2=scala.collection.mutable.HashMap.empty[String, String ]
     val map3=scala.collection.mutable.HashMap.empty[String, NameCount ]
     for ( i <- 0 to ratingA.size-1 ) {
        val myid= ratingA(i).product_id.get  
        if ( map.contains(myid) ) {
            map(myid) += 1
        } else {
            map += ( myid ->1 )
            map2 += ( myid ->myid )
        }
     }

     var myL = List[NameCount]()
     map foreach { case ( key, value ) =>
        myL ::= ( new NameCount (  count=Option(value), name=Option(map2(key))   ) )  
     } 


     var myL2 = List[ProductReport]()
     map foreach { case ( key, value ) =>
           myL2 ::= ( new ProductReport (  
                      total=Option(value), 
                      name=Option(map2(key)) ,
                      rating=getReviewsSum(ClientID, key)
                ) )  
     }

     myL2 
 } 


  def getReviewsSum(ClientID: String, ProductID: String) = {
     val condObject= MongoDBObject (  "client_id"->1, "product_id"->1 , "review_rating"->1 ); 
     val tObject= MongoDBObject ( "$exists" ->true ); 

     val rating4 = dao.find(MongoDBObject("client_id" -> ClientID, "product_id" -> ProductID ,  "review_rating"->tObject  ), condObject )
     val ratingA=rating4.toArray;

     val map=scala.collection.mutable.HashMap.empty[String, Int ]
     if ( ratingA.size >0 ) {
         for ( i <- 0 to ratingA.size-1 ) {
            val myid= ratingA(i).review_rating.get  
            if ( map.contains(myid) ) {
               map(myid) += 1
            } else {
                map += ( myid ->1 )
            }

         }
     }

     map 
     var myL = List[NameCount]()
     map foreach { case ( key, value ) =>
        myL ::= ( new NameCount (  count=Option(value), name=Option(key)   ) )  
     } 

     myL 
     
  }

/*
  def getReviewsSum(ClientID: String, ProductID: String) = {
     val condObject= MongoDBObject (  "client_id"->1, "product_id"->1 , "review_rating"->1 ); 
     val tObject= MongoDBObject ( "$exists" ->true ); 

     val rating4 = dao.find(MongoDBObject("client_id" -> ClientID, "product_id" -> ProductID ,  "review_rating"->tObject  ), condObject )
     val ratingA=rating4.toArray;

     val map=scala.collection.mutable.HashMap.empty[String, Int ]
     if ( ratingA.size >0 ) {
         for ( i <- 0 to ratingA.size-1 ) {
            val myid= ratingA(i).review_rating.get  
            if ( map.contains(myid) ) {
               map(myid) += 1
            } else {
                map += ( myid ->1 )
            }

         }
     }

     map 
     var myL = List[NameCount]()
     map foreach { case ( key, value ) =>
        myL ::= ( new NameCount (  count=Option(value), name=Option(key)   ) )  
     } 

     myL 
     
  }

  */



}
