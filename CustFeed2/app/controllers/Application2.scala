package controllers

import play.api.libs.json.Json
import play.api.libs.json.JsValue
import play.api.libs.json.JsArray
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.Play.current
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.PlayBsonImplicits._
import play.modules.reactivemongo.ReactiveMongoPlugin
import reactivemongo.api.QueryBuilder
import reactivemongo.api.SortOrder
import reactivemongo.bson._
import reactivemongo.bson.handlers.DefaultBSONHandlers._
import play.api.libs.json.Reads

object Application2 extends Controller with MongoController {
  val db = ReactiveMongoPlugin.db
  val review_collection = db("reviews")
  val feedback_collection = db("feedback")

  
  def getReview(id: String) = Action { implicit request =>
    Async {
      val oid = Json.obj("$oid" -> id)
      val qb = QueryBuilder().query(Json.obj( "_id" -> oid ))
      
      review_collection.find[JsValue]( qb ).toList.map { reviews =>
        Ok(reviews.foldLeft(JsArray(List()))( (obj, review) => obj ++ Json.arr(review) ))
      }
    }
  }
  
  def getReviews(client_id: String, product_id: String) = Action { implicit request =>
    Async {
      val qb = QueryBuilder().query(Json.obj( "client_id" -> client_id, "product_id" -> product_id ))
      
      review_collection.find[JsValue]( qb ).toList.map { reviews =>
        Ok(reviews.foldLeft(JsArray(List()))( (obj, review) => obj ++ Json.arr(review) ))
      }
    }
  }
  
  def addReview(client_id: String, product_id: String) = Action(parse.json) {  request =>
    Async {
//      val review = Json.obj("client_id" -> client_id, "product_id" -> product_id)   
      review_collection.insert[JsValue]( request.body ).map( lastError =>
        Ok("Mongo LastErorr:%s".format(lastError))
      )
    }
  }
  
  def addReviewFeedback(review_id: String) = Action(parse.json) { request =>
    Async {
      feedback_collection.insert[JsValue]( request.body ).map( lastError =>
        Ok("Mongo LastErorr:%s".format(lastError))
      )  
    } 
  }
}