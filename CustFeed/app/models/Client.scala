package models

case class Client ( client_id:String, contact:String, url:String )

object Client {
   def all(): List[Client] =Nil
}
