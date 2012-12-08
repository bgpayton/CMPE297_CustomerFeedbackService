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

case class SysINFO(
  _id: ObjectId = new ObjectId,
  update_time: Option[String] = None, 
  update_day: Option[String] = None, 
  memory_used_M: Option[String] = None,
  memory_total_M: Option[String] = None,
  memory_free_M: Option[String] = None,

  play_session: Option[Int] = None,
  mongod_session: Option[Int] = None

)

object SysINFO extends ModelCompanion[SysINFO, ObjectId] {
  val collection = mongoCollection("reviews")
  val dao = new SalatDAO[SysINFO, ObjectId](collection = collection) {}
  val retC = dao.find(MongoDBObject()  )  
 
  def getFew = {
     val condObject= MongoDBObject ( );
     val tObject= MongoDBObject ( "$exists" ->true );

     val ret = dao.find(MongoDBObject( "play_session" -> tObject ) )
     val retA=ret.toArray;

     retA

  }


}
