package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.collection.mutable.Stack
import play.api.libs.json.Json
import play.api.libs.json._
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal


case class TreeNode (v: Int, s: String) {
  var value: Int = v
  var symbol: String = s
  var left: TreeNode = null
  var right: TreeNode = null
}

@Singleton
class CalculusController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {  
   
  def calculus(query: String): Action[AnyContent] = {
    Action { implicit request =>
     try{
       //have to get it from raw as it doesn't pass '+' sign (treats it as space) and encoding doesn't help 
       val rawquery = request.rawQueryString       
       val encodedQuery = encodeRawQuery(rawquery)      
       val decodedExpression = getUTF8Query(encodedQuery)
       val cleanedExpression = cleanQuery(decodedExpression)
       val RPNExpression = convertToRPNExpression(cleanedExpression)
       val result = calculate(RPNExpression)
       Ok(getResultJson(false, Some(result), null))     
     }
     catch{
       case NonFatal(e)=> Ok(getResultJson(true, null, e.toString()))
     }
    }
  } 
  
  private def encodeRawQuery(query: String) : String = {
    //split in case user sends any other parameters after query
       val queryOnly = query.replace("%20", "").replace("query=", "").split("&")(0)
       val encoder :org.apache.catalina.util.URLEncoder = new org.apache.catalina.util.URLEncoder
       encoder.addSafeCharacter('-')
       encoder.addSafeCharacter('+')
       encoder.addSafeCharacter('/')
       encoder.addSafeCharacter('*')
       encoder.addSafeCharacter('(')
       encoder.addSafeCharacter(')')
       encoder.addSafeCharacter('=') //needed for base64
       return encoder.encode(queryOnly)      
  }
  
  private def getUTF8Query(query: String) : String = {
    if(query.isEmpty){
      throw new IllegalArgumentException("Query is empty.")   
    }
    val base64RegEx = """^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""".r   
    val isBase64 = base64RegEx.pattern.matcher(query).matches
    //if-else is more efficient for booleans than matching in scala
    if(isBase64)return decodeBase64toUTF8(query)
    else checkForIlligalStringinUTF8(query)      
  }
  
  private def checkForIlligalStringinUTF8(query: String) : String = {
      val QueryRegEx = """[0-9\+\/\*\/\-\s+\()]+""".r   
      val isExpressionOk = QueryRegEx.pattern.matcher(query).matches
      if(isExpressionOk)return query
      else throw new IllegalArgumentException("Invalid input. UTF-8: Please, make sure to use only numbers (0-9) and special characters including +-/*(); Base64: please check the allowed character before encoding.")
  }
  
  private def decodeBase64toUTF8(expression: String) : String = {
    val decoded = Base64.getDecoder.decode(expression)
    val stringQuery = new String(decoded, StandardCharsets.UTF_8)    
    return checkForIlligalStringinUTF8(stringQuery)
  }
  
  private def cleanQuery(expression: String) : ArrayBuffer[String] = {  
    //remove all empty spaces between letters 
    val nospaces = expression.replaceAll("\\s+", "")    
    //remove all chars except +-*/() and digits 
    val onlyallowedsymbols = nospaces.replaceAll("[^\\(\\)\\*\\+\\-\\d/\\s]", "")  
    //separate digits from symbols to array
    val listofsymbols = onlyallowedsymbols.split("(?<=[-+*/()])|(?=[-+*/()])") 
    return ArrayBuffer(listofsymbols : _*)
  }  
  
  private def convertToRPNExpression(expression: ArrayBuffer[String]) : ArrayBuffer[String] = {   
    val resultArray = ArrayBuffer[String]()
    val root :TreeNode = createNodeTreeForRPN(expression)
    return getCharactersInRPEOrder(resultArray, root) 
  }
  
  private def calculate(expression: ArrayBuffer[String]) : Double = {
    try {
    val stack = Stack[Double]()
    for(token <- expression){
       token match{
        case "+" => 
          stack.push(stack.pop() + stack.pop())   
        case "-" => 
          stack.push(-stack.pop() + stack.pop()) 
        case "*" => 
          stack.push(stack.pop() * stack.pop())
        case "/" => 
          val divisor = stack.pop().toDouble
          stack.push(stack.pop() / divisor)
        case "" => //do nothing if empty string
        case _ =>  stack.push(token.toDouble)
      }    
    }   
    return stack.pop()
    }
    catch{
      case NonFatal(e)=> throw new IllegalArgumentException("Problem with calculations has occurred. Please check if the expression is written correctly.")       
    }
  }  
  
  private def createNodeTreeForRPN(expression: ArrayBuffer[String]) : TreeNode = {
    val stack = Stack[TreeNode]()
    var base :Int = 0
    var value :Int = 0
    
   for(token <- expression){
     if (token == "(") {
                base += 10
            }
     if (token == ")") {
                base -= 10
            }
     value = getWeight(base, token)
     val node = new TreeNode(value, token)
     while (!stack.isEmpty && node.value <= stack.top.value) {
                node.left = stack.pop()
            }
     if (!stack.isEmpty) {
                stack.top.right = node
            }
     stack.push(node)
    }
    if (stack.isEmpty) {
            throw new IllegalArgumentException("Problem with calculations has occurred. Please check if the expression is written correctly.")
    }
     var rst :TreeNode = stack.pop() 
        while (!stack.isEmpty) {
            rst = stack.pop()
        }
        return rst
  }
  
  private def getWeight(base: Int, s: String) : Int = {  
    if (s == "+" || s == "-") {
        return base + 1
        }
    if (s == "*" || s == "/") {
        return base + 2
    }
    return Integer.MAX_VALUE   
  }
  
  private def getCharactersInRPEOrder(rst: ArrayBuffer[String], node: TreeNode) : ArrayBuffer[String] = {
  	if (node.left != null) {
  	    getCharactersInRPEOrder(rst, node.left)
  	}
  	if (node.right != null) {
  		  getCharactersInRPEOrder(rst, node.right)
  	}
  	
     node.symbol match{
      case "(" =>     
      case ")" =>     
      case _ => rst += node.symbol 
    }
    return rst
  } 
  
  private def getResultJson(error :Boolean, result :Some[Double], message :String) : JsValue = {      
      if(error) {
        return Json.obj(
        "error" -> error,
        "message" -> message) 
      }
      else {
        return Json.obj(
        "error" -> error,
        "result" -> result)  
      }
  }
}