package org.qbproject.schema.json

import org.qbproject.schema.QBSchema._
import org.qbproject.schema.{QBClass, QBValidator}
import org.scalameter.api._
import play.api.libs.json.Json._
import play.api.libs.json._

import scala.util.Random


object ValidationBenchmark extends PerformanceTest {

  /* configuration */
  override lazy val reporter = new LoggingReporter

  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val persistor = Persistor.None


  val numberOfInstancesGen = Gen.range("numberOfInstances")(500, 5000, 500)

  val instances = for {
    numberOfInstances <- numberOfInstancesGen
  } yield BenchmarkInstanceGenerator.generate(numberOfInstances)

  performance of "QBValidator" in {
    measure method "validate" in {
      using(instances) in { t =>
        validatorRun(t)
      }
    }
  }

  def validatorRun(objs: List[JsObject]) = {
    objs.map {
      instance =>
        QBValidator.validate(BenchmarkSchema.auto)(instance)
    }
  }

  def generate(depth: Int, fieldsPerObject: Int): (QBClass, JsObject) = {
    val start = (qbClass(), obj())
    if (depth == 0) {
      start
    } else {
      (0 to fieldsPerObject)
        .foldLeft(start) {
        (t: (QBClass, JsObject), idx: Int) =>
          val name = "field" + idx

          val sub = idx % 5 match {
            case 0 => (qbBoolean, JsBoolean(true))
            case 1 => (qbNumber(), JsNumber(42))
            case 2 => (qbString, JsString("Some Text"))
            case 3 =>
//              val subsub = generate(depth - 1, fieldsPerObject)
              (qbList(qbClass()), JsArray((0 to fieldsPerObject).map({ x: Int => obj()})))
            case 4 => generate(depth - 1, fieldsPerObject)
          }

          (t._1 ++ qbClass(name -> sub._1), t._2 + (name -> sub._2))
      }
    }
  }

}

object BenchmarkInstanceGenerator {

  import play.api.libs.json.Json._

  val r = new Random(12345)

  def generate(): JsObject = {
    obj(
      "meta" -> obj(
        "name" -> generateString(10),
        "make" -> generateString(15),
        "year" -> generateNumberString(4),
        "price" -> r.nextInt()
      ),
      "extra" -> arr(
        obj(
          "name" -> generateString(10),
          "description" -> generateString(1000),
          "price" -> r.nextInt()
        )
      ),
      "tires" -> arr(
        obj(
          "diameter" -> r.nextInt(30),
          "width" -> r.nextInt(20),
          "color" -> "red",
          "material" -> generateString(10)
        )
      ),
      "technicalData" -> obj(
        "engine" -> obj(
          "capacity" -> r.nextInt(),
          "torque" -> r.nextInt(),
          "power" -> r.nextInt()
        ),
        "maxVelocity" -> r.nextInt(),
        "weight" -> r.nextInt()
      ),
      "interior" -> obj(
        "colorOne" -> generateString(10),
        "colorTwo" -> generateString(10),
        "colorThree" -> generateString(10)
      ),
      "exterior" -> obj(
        "colorOne" -> generateString(10),
        "colorTwo" -> generateString(10)
      )
    )
  }

  def generate(cnt: Int): List[JsObject] = {
    (0 until cnt).map {
      idx =>
        generate()
    }.toList
  }

  def generateString(cnt: Int): String = {
    r.nextString(cnt)
  }

  def generateNumberString(cnt: Int): String = {
    val builder = new StringBuilder()
    for (i <- 0 until cnt) {
      builder.append(r.nextInt(10))
    }
    builder.toString()
  }

}

object BenchmarkSchema {

  val tire = qbClass(
    "diameter" -> qbNumber,
    "width" -> qbNumber,
    "color" -> qbString,
    "material" -> qbString
  )

  val color = qbEnum("red", "blue", "green", "yellow", "magenta", "cyan")

  val auto = qbClass(
    "meta" -> qbClass(
      "name" -> qbString,
      "make" -> qbString,
      "year" -> qbString,
      "price" -> qbNumber
    ),
    "extra" -> qbList(qbClass(
      "name" -> qbString,
      "description" -> qbText,
      "price" -> qbNumber
    )),
    "tires" -> qbList(tire),
    "technicalData" -> qbClass(
      "engine" -> qbClass(
        "capacity" -> qbNumber,
        "torque" -> qbNumber,
        "power" -> qbNumber
      ),
      "maxVelocity" -> qbNumber,
      "weight" -> qbNumber

    ),
    "interior" -> qbClass(
      "colorOne" -> color,
      "colorTwo" -> color,
      "colorThree" -> color
    ),
    "exterior" -> qbClass(
      "colorOne" -> color,
      "colorTwo" -> color
    ),
    "objects" -> createObject(2, 10)
  )

  def createObject(depth: Int, width: Int): QBClass = {
    if (depth <= 0) {
      qbClass(
        Range(0, width).map {
          idx =>
            idx.toString -> qbString
        }: _ *
      )
    } else {
      qbClass(
        Range(0, width).map {
          idx =>
            idx.toString -> createObject(depth - 1, width)
        }: _ *
      )
    }
  }

}
