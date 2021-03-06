package org.qbproject

import java.util.regex.Pattern

import org.qbproject.schema._
import play.api.data.validation.ValidationError
import play.api.libs.json._

import scalaz.{Failure, Success, Validation}

package object mongo {

  class QBObjectId(rules: Set[ValidationRule[JsString]]) extends QBStringImpl(rules) {
    override def toString = "objectId"
  }

  object ObjectIdRule extends FormatRule[JsString] {
    override def format: String = "objectId"

    val pattern = Pattern.compile("[0-9A-Fa-f]{24}")

    override def validate(a: JsString): Validation[ERRORS, JsString] =
      if (pattern.matcher(a.value).matches()) {
        Success(a)
      } else {
        Failure(List(ValidationError(s"'$a' doesn't comply with RegEx '${pattern.pattern()}'")))
      }
  }

  def objectId = new QBObjectId(Set(ObjectIdRule))

  def objectId(endpoint: String) = new QBObjectId(Set(ObjectIdRule, new KeyValueRule("endpoint", endpoint)))

  def read(schema: QBClass)(instance: JsObject): JsResult[JsObject] = {
    new MongoTransformer(schema).fromMongoJson(instance)
  }

  def write(schema: QBClass)(instance: JsObject): JsResult[JsObject] = {
    new MongoTransformer(schema).toMongoJson(instance)
  }

  object toMongoId extends (JsObject => JsObject) {
    override def apply(jsObject: JsObject): JsObject = JsObject(jsObject.fields.map {
      case ("id", value) => ("_id", value)
      case fd => fd
    })
  }

  object fromMongoId extends (JsObject => JsObject) {
    override def apply(jsObject: JsObject): JsObject = JsObject(jsObject.fields.map {
      case ("_id", value) => ("id", value)
      case fd => fd
    })
  }
}
