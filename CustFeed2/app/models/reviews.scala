package models

import org.jboss.netty.buffer._
import org.joda.time.DateTime
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._

import reactivemongo.bson._
import reactivemongo.bson.handlers._

case class Review(
  id: Option[BSONObjectID],
  client_id: String,
  product_id: String,
  reviewer_id: Option[String],
  review_rating: Option[String],
  review_summary: Option[String],
  review_details: Option[String],
  review_date: DateTime
)

object Review {
  implicit object ReviewBSONReader extends BSONReader[Review] {
    def fromBSON(document: BSONDocument) :Review = {
      val doc = document.toTraversable
      Review(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[BSONString]("client_id").get.value,
        doc.getAs[BSONString]("product_id").get.value,
        doc.getAs[BSONString]("reviewer_id").map(ri => new String),
        doc.getAs[BSONString]("review_rating").map(rr => new String),
        doc.getAs[BSONString]("review_summary").map(rs => new String),
        doc.getAs[BSONString]("review_details").map(rd => new String),
        new DateTime(doc.getAs[BSONDateTime]("review_date").get.value)
      )
    }
  }
  implicit object ReviewBSONWriter extends BSONWriter[Review] {
    def toBSON(review: Review) = {
      BSONDocument(
        "_id" -> review.id.getOrElse(BSONObjectID.generate),
        "client_id" -> BSONString(review.client_id),
        "product_id" -> BSONString(review.product_id),
        "reviewer_id" -> review.reviewer_id.map(ri => BSONString(review.reviewer_id.get)),
        "review_rating" -> review.review_rating.map(rr => BSONString(review.review_rating.get)),
        "review_summary" -> review.review_summary.map(rs => BSONString(review.review_summary.get)),
        "review_details" -> review.review_details.map(rd => BSONString(review.review_details.get)),
        "review_date" -> BSONDateTime(review.review_date.getMillis()))
    }
  }
  val form = Form(
    mapping(
      "id" -> optional(of[String] verifying pattern(
        """[a-fA-F0-9]{24}""".r,
        "constraint.objectId",
        "error.objectId")),
      "client_id" -> nonEmptyText,
      "product_id" -> nonEmptyText,
      "reviewer_id" -> optional(of[String]),
      "review_rating" -> optional(of[String]),
      "review_summary" -> optional(of[String]),
      "review_details" -> optional(of[String])
    ) { (id, client_id, product_id, reviewer_id, review_rating, review_summary, review_details) =>
      Review(
        id.map(new BSONObjectID(_)),
        client_id,
        product_id,
        reviewer_id.map(new String(_)),
        review_rating.map(new String(_)),
        review_summary.map(new String(_)),
        review_details.map(new String(_)),
        new DateTime
      )
    } { (review =>
      Some(
        (review.id.map(_.stringify),
        review.client_id,
        review.product_id,
        review.reviewer_id.map(_.toString()),
        review.review_rating.map(_.toString()),
        review.review_summary.map(_.toString()),
        review.review_details.map(_.toString())
      )))
    })
}

case class ReviewFeedback(
  id: Option[BSONObjectID],
  review_id: String,
  user_id: String,
  feedback: Integer
)

object ReviewFeedback {
  implicit object ReviewFeedbackBSONReader extends BSONReader[ReviewFeedback] {
    def fromBSON(document: BSONDocument) : ReviewFeedback = {
      val doc = document.toTraversable
      ReviewFeedback(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[BSONString]("review_id").get.value,
        doc.getAs[BSONString]("user_id").get.value,
        doc.getAs[BSONInteger]("feedback").get.value
      )
    }
  }
  implicit object ReviewFeedbackBSONWriter extends BSONWriter[ReviewFeedback] {
    def toBSON(reviewFeedback: ReviewFeedback) = {
      BSONDocument(
        "_id" -> reviewFeedback.id.getOrElse(BSONObjectID.generate),
        "review_id" -> BSONString(reviewFeedback.review_id),
        "user_id" -> BSONString(reviewFeedback.user_id),
        "feedback" -> BSONInteger(reviewFeedback.feedback)
      )
    }
  }
}
     