package reviewDB;

import java.util.ArrayList;
import java.util.List;

import com.mongodb.BasicDBObject;
import com.mongodb.DBCollection;
import com.mongodb.DBCursor;

public class Product {
	
	private String product_id; 
	private String client_id; 
	
	public Product (String pid, String cid ) {
		product_id=pid; 
		client_id=cid; 
	}
	
	public DBCursor getReviewData(DBCollection coll) {
		List <ReviewData>  retL= new ArrayList<ReviewData>(); 
		BasicDBObject q= new BasicDBObject();
		q.put("product_id", product_id); 
		q.put("client_id", client_id); 
		System.out.println(q); 
		
		DBCursor cursor =coll.find(q);
		
		return cursor; 
		
	}

}
