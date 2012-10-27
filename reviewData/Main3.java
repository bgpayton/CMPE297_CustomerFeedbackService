package reviewDB;

import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Set;

import com.mongodb.BasicDBObject;
import com.mongodb.DB;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;
import com.mongodb.Mongo;

public class Main3 {

	/**
	 * @param args
	 */
	
	public static DBCollection coll; 
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		MongoHelper m = new MongoHelper(); 
		coll = m.getCollectiono(); 
		
		Product prod=new Product(  "prod_a4", "cust_b"); 
		DBCursor cursor=prod.getReviewData(coll) ; 
		
		try {
			int i=1; 
			while (cursor.hasNext()) {
				DBObject obj=cursor.next(); 
				System.out.println( " db: " + i + "\n" + obj ); 
				i++; 
			}
		} finally {
			cursor.close(); 
		}
	 
		/*
		Hashtable<String, ReviewData> hTable=new Hashtable<String, ReviewData>(); 
		
    	ReviewData rd=new ReviewData("cust_999", "bus_01", "review_text", "prod_db2");  
		
	 	BasicDBObject doc = rd.getMongoDoc(); // convert to db format and put 			
    //	coll.insert(doc); 
   	
    	ReviewData retrd=null; 
		
		DBCursor cursor=coll.find(); 
		try {
			int i=1; 
			while (cursor.hasNext()) {
				DBObject obj=cursor.next(); 
				retrd=new ReviewData(obj); 
				String key=retrd.getKey(); 
				hTable.put(key, retrd); 
				i++; 
			}
		} finally {
			cursor.close(); 
		}
		
	
		
		Enumeration em=hTable.keys(); 
		int j=1; 
		while (em.hasMoreElements()) {
			String em_key=(String) em.nextElement(); 
			rd=(ReviewData) hTable.get(em_key);   //rd.toString()  rd.getDoc()
			System.out.println( "return RD= "+ j + " review_id= " + em_key  + "\t" + 
			                     rd.getReviewText() );
			j++; 
		}
		
		
		String update_key="1350868288"; 
		ReviewData update_rd=hTable.get(update_key); 
		update_rd.updateReviewText("new txt", coll); 
		update_rd.inc_positive(coll); 
		update_rd.inc_negative(coll); 
		
		
		
		System.out.println("After update"); 
		
		
		j=1; 
		
		
		cursor=coll.find();
		try {
			int i=1; 
			while (cursor.hasNext()) {
				DBObject obj=cursor.next(); 
				System.out.println( " db: " + i + "\n" + obj ); 
				i++; 
			}
		} finally {
			cursor.close(); 
		}
		
	*/
		
	}

}
