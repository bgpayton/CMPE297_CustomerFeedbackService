package reviewDB;


import java.net.UnknownHostException;

import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.Mongo;


public class MongoHelper {
	Mongo m;
	DB db ; 
	DBCollection coll; 
	
	public MongoHelper(){
	   try {
		   m = new Mongo( "localhost" , 27017 );
		   db = m.getDB( "custFeed" );
		   coll = db.getCollection("reviews"); 
	   } catch (UnknownHostException e) {
		  // TODO Auto-generated catch block
		  e.printStackTrace();
	   }
	}
	

	public DBCollection getCollectiono(){
	   return (coll); 
	}
	
	
	
}
