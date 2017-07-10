package controllers

import scala.concurrent.Future

import org.scalatestplus.play._

import play.api.mvc._
import play.api.test._
import play.api.test.Helpers._
import play.api.mvc.AnyContentAsJson
import play.api.libs.json.Json
import play.api.libs.json._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class CalculusControllerSpec extends PlaySpec with Results {

  "Calculus controller culculus method" should {

    "Calculate expression 10-5 correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "10-5"
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":5}"""
    }   
    
    "Calculate expression 5+6 correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "5+6"
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":11}"""
    }   
    
    "Calculate expression 12/2-3 correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "12/2-3"
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":3}"""
    }  
    
    "Calculate expression 20+8*(1+2) correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "20+8*(1+2)"
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":44}"""
    } 
    
    "Calculate expression 2 * (23/(3*3))- 23 * (2*3) correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "2%20*%20(23/(3*3))-%2023%20*%20(2*3)" //as it will be decoded by browser
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":-132.88888888888889}"""
    } 
    
    "Calculate expression MiAqICgyMy8oMyozKSktIDIzICogKDIqMyk= correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "MiAqICgyMy8oMyozKSktIDIzICogKDIqMyk="
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":-132.88888888888889}"""
    }  
    
    "Calculate expression MiooMis5KS0oMys1KQ== correctly" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "MiooMis5KS0oMys1KQ=="
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":false,"result":14}"""
    }  
    
    "Throw an error about invalid input when passed with query parameter 'MiooMris5KS0oMys1KQ==' (additional letter in the middle)" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "MiooMris5KS0oMys1KQ=="
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":true,"message":"java.lang.IllegalArgumentException: Invalid input. UTF-8: Please, make sure to use only numbers (0-9) and special characters including +-/*(); Base64: please check the allowed character before encoding."}"""
    } 
    
    "Throw an error about invalid input when passed with query parameter '(hi!3+3)*8'" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = "(hi!3+3)*8"
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":true,"message":"java.lang.IllegalArgumentException: Invalid input. UTF-8: Please, make sure to use only numbers (0-9) and special characters including +-/*(); Base64: please check the allowed character before encoding."}"""
    } 
    
    "Throw an error about empty string)" in {
      val controller = new CalculusController(stubControllerComponents())
      val query :String = ""
      val result: Future[Result] = controller.calculus(query).apply(FakeRequest(GET, "/calculus?query=" + query))
      val bodyText: String = contentAsString(result)
      bodyText mustBe """{"error":true,"message":"java.lang.IllegalArgumentException: Query is empty."}"""
    } 
  }
}
