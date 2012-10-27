package reviewDB;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Random;

import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;
import com.mongodb.DBObject;

public class Mpopulate {

	/**
	 * @param args
	 */
	public static final String DATA_FORMAT_NOW="yyyy-MM-dd HH:mm:ss" ; 
	public static DBCollection coll; 
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String reviewer="reviewer_"; 
		String client="client_"; 
		String review_text="review_text"; 
		String product="prod_";
		int rating; 
		
		MongoHelper m = new MongoHelper(); 
		coll = m.getCollectiono(); 
		
		Random rg=new Random(); 
		System.out.println( "reviewer  " + "client  "  +
		         " review_text  "  + " product " ); 
		
		   client = "cust_b"  ; 
		   for ( int pr=1; pr<5; pr++ ) {
			 product = "prod_a" + pr; 
		     for ( int i=1 ; i< 5; ++i) {
			     int rand=rg.nextInt(30); 
			      reviewer = "reviewer_" +  rand; 
			      rating=rg.nextInt(10)+ 1; 
			      int d=rg.nextInt(200); 
			      Calendar cal= Calendar.getInstance();
			      cal.add(Calendar.DATE, d*(-1) ); 
			      cal.add(Calendar.HOUR, d*(-1) );
				  SimpleDateFormat sdf=new SimpleDateFormat(DATA_FORMAT_NOW); 
				  String review_date= sdf.format(cal.getTime()); 
				  ReviewData rd=new ReviewData(reviewer, client, review_text, product, rating, review_date); 
	//		      System.out.println( reviewer + " " + client + " "+ 
	//		          review_text + " "+ product + " " + rating + " "+ review_date); 
			      BasicDBObject doc = rd.getMongoDoc();
			      System.out.println (doc); 
			      coll.insert(doc); 
		      }
		   }  
		
		   System.out.println(" ==============="); 

		   DBCursor cursor=coll.find();
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
		
		
	}

}
