package reviewDB;

import java.text.SimpleDateFormat;
import java.util.Calendar;

import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;


public class ReviewData {
	
	private String review_id; 
	private int rating; 
	private String reviewer_id;
	private String product_id; 
	private String client_id; 
	private String review_date;
	private String review_text;
	private int positive_count; 
	private int negative_count; 
	private int inappropriate_count; 
	private BasicDBObject doc;
	
	public static final String DATA_FORMAT_NOW="yyyy-MM-dd HH:mm:ss"; 

	public ReviewData(String rer_id, String c_id, String r_text, String p_id ){
		long epoch =  System.currentTimeMillis()/1000 ; 
		review_id = Long.toString(epoch); 
		rating=0; 
		reviewer_id=rer_id; 
		client_id=c_id; 
		review_text=r_text;
		product_id=p_id; 
		
		Calendar cal= Calendar.getInstance();
		SimpleDateFormat sdf=new SimpleDateFormat(DATA_FORMAT_NOW); 
		review_date= sdf.format(cal.getTime()); 
		positive_count=0; 
		negative_count=0; 
		inappropriate_count=0; 
		
	}
	
	public ReviewData(String rer_id, String c_id, String r_text, String p_id , int r , String d){
		long epoch =  System.currentTimeMillis()/1000 ; 
		review_id = Long.toString(epoch);
		rating=r;
		reviewer_id=rer_id; 
		client_id=c_id; 
		review_text=r_text;
		product_id=p_id; 
		review_date= d ; 
		positive_count=0; 
		negative_count=0; 
		inappropriate_count=0; 
		
	}
	
	
	public ReviewData(DBObject myDoc){
		String me="ReviewData ";
		doc=(BasicDBObject) myDoc; 
		
		review_id=  myDoc.get("review_id").toString(); 
		reviewer_id=  myDoc.get("reviewer_id").toString(); 
		rating=  Integer.parseInt( myDoc.get("rating").toString() ); 
		client_id= myDoc.get("client_id").toString();  
		product_id= myDoc.get("product_id").toString(); 
		review_date= myDoc.get("review_date").toString(); ;
		review_text= myDoc.get("review_text").toString(); ;
		positive_count= Integer.parseInt(myDoc.get("positive_count").toString() ); 
		negative_count= Integer.parseInt(myDoc.get("negative_count").toString() ); 
		inappropriate_count= Integer.parseInt( myDoc.get("inappropriate_count").toString() ); ; 
	}
	
	
	public String getKey(){
		return (review_id); 
	}
	
	public String getReviewText(){
		return (review_text); 
	}
	
	public BasicDBObject getDoc(){
		return (doc); 
	}
	
	public String toString() {
		String ret=""; 
		ret = " review_id=" + review_id + "\n" ; 
		ret += " rating=" + rating + "\n" ; 
		ret += " reviewer_id=" + reviewer_id + "\n" ;
		ret += " client_id=" + client_id + "\n"  ; 
		ret += " product_id=" + product_id + "\n"; 
		ret += " review_date="+ review_date + "\n"  ; 
		ret += " review_text=" + review_text + "\n"  ;
		ret += " positive_count=" + positive_count + "\n" ; 
		ret += " negative_count= " + negative_count + "\n" ; 
		ret += " inappropriate_count=" + inappropriate_count + "\n" ; 
		return (ret); 
		
	}
	
	public BasicDBObject getMongoDoc(){
		doc = new BasicDBObject(); 
		doc.put("review_id", review_id); 
		doc.put("rating", rating); 
		doc.put("reviewer_id", reviewer_id); 
		doc.put("client_id", client_id);
		doc.put("product_id", product_id); 
		doc.put("review_date", review_date);
		doc.put("review_text", review_text);
		doc.put("positive_count", positive_count);
		doc.put("negative_count", negative_count);
		doc.put("inappropriate_count", inappropriate_count);	
		return (doc); 
		
	}
	
	public void updateReviewText (String text, DBCollection coll){
		BasicDBObject newDocument3 = new BasicDBObject().append("$set", 
				new BasicDBObject().append("review_text", text));
		review_text=text;
		doc.put("review_text", review_text);
		coll.update(new BasicDBObject().append("review_id", review_id), newDocument3);
	}
	
	public void inc_positive(DBCollection coll) {
		BasicDBObject newDocument = new BasicDBObject().append("$inc", 
				new BasicDBObject().append("positive_count", 1));	 
			coll.update(new BasicDBObject().append("review_id", review_id), newDocument);
			
	}
	
	public void inc_negative(DBCollection coll) {
		BasicDBObject newDocument = new BasicDBObject().append("$inc", 
				new BasicDBObject().append("negative_count", 1));	 
			coll.update(new BasicDBObject().append("review_id", review_id), newDocument);
	}
	
	public void inc_inappropriate(DBCollection coll) {
		BasicDBObject newDocument = new BasicDBObject().append("$inc", 
				new BasicDBObject().append("negative_count", 1));	 
			coll.update(new BasicDBObject().append("review_id", review_id), newDocument);
	}
	

}
