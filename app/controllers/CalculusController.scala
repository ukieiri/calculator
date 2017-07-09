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


@Singleton
class CalculusController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {  
   
  def calculus(query: String): Action[AnyContent] = {
    Action { implicit request =>
     try{
       //have to get it from raw as it doesn't pass '+' sign (treats it as space) and encoding doesn't help 
       val rawquery = request.rawQueryString       
       var encodedQuery = encodeRawQuery(rawquery)      
       val decodedExpression = getUTF8Query(encodedQuery)
       var cleanedExpression = cleanQuery(decodedExpression)
       var RPNExpression = convertToRPNExpression(cleanedExpression)
       var result = calculate(RPNExpression)
       Ok(getResultJson(false, result, null))
     }
     catch{
       case e: Throwable => Ok(getResultJson(true, 1.0, e.toString()))
     }
    }
  } 
  
  def encodeRawQuery(query: String) : String = {
    //split in case user sends any other parameters after query
       var queryOnly = query.replace("%20", "").replace("query=", "").split("&")(0)
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
  
  def getUTF8Query(query: String) : String = {
    if(query.isEmpty){
      throw new IllegalArgumentException("Query is empty.")   
    }
    val base64RegEx = """^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""".r   
    val isBase64 = base64RegEx.pattern.matcher(query).matches
    val string = isBase64 match{
        case true => return decodeBase64toUTF8(query)
        case false => checkForIlligalStringinUTF8(query)
    }      
    return query
  }
  
  def checkForIlligalStringinUTF8(query: String) : String = {
      val QueryRegEx = """[0-9\+\/\*\/\-\s+\()]+""".r   
      val isExpressionOk = QueryRegEx.pattern.matcher(query).matches
      val string = isExpressionOk match{
        case true => return query
        case false => throw new IllegalArgumentException("Please, make sure to use only numbers (0-9) and special characters including +-/*()")   
    }  
    return query
  }
  
  def cleanQuery(expression: String) : ArrayBuffer[String] = {  
    //remove all empty spaces between letters 
    val nospaces = expression.replaceAll("\\s+", "")    
    //remove all chars except +-*/() and digits 
    val onlyallowedsymbols = nospaces.replaceAll("[^\\(\\)\\*\\+\\-\\d/\\s]", "")  
    //separate digits from symbols to array
    val listofsymbols = onlyallowedsymbols.split("(?<=[-+*/()])|(?=[-+*/()])") 
    return ArrayBuffer(listofsymbols : _*)
  }
  
  def decodeBase64toUTF8(expression: String) : String = {
    val decoded = Base64.getDecoder.decode(expression)
    val stringQuery = new String(decoded, StandardCharsets.UTF_8)    
    return checkForIlligalStringinUTF8(stringQuery)
  }
  
  def convertToRPNExpression(expression: ArrayBuffer[String]) : ArrayBuffer[String] = {   
    val resultArray = ArrayBuffer[String]()
    var root :TreeNode = createNodeTreeForRPN(expression)
    val res = getSymbols(resultArray, root) 
    return res
  }
  
  def calculate(expression: ArrayBuffer[String]) : Double = {
    try {
    var stack = Stack[Double]()
    for(token <- expression){
       token match{
        case "+" => 
          stack.push(stack.pop() + stack.pop())   
        case "-" => 
          stack.push(-stack.pop() + stack.pop()) 
        case "*" => 
          stack.push(stack.pop() * stack.pop())
        case "/" => 
          var divisor = stack.pop().toDouble
          stack.push(stack.pop() / divisor)
        case "" => //do nothing if empty string
        case _ =>  stack.push(token.toDouble)
      }    
    } 
  
    return stack.pop()
    }
    catch{
      case e: Throwable => throw new IllegalArgumentException("Problem with calculations has occurred. Please check if the expression is written correctly.")       
    }
  }
  
  class TreeNode (v: Int, s: String) {
    var value: Int = v
    var symbol: String = s
    var left: TreeNode = null
    var right: TreeNode = null
  }
  
  def createNodeTreeForRPN(expression: ArrayBuffer[String]) : TreeNode = {
    if (expression == null || expression.length == 0) {
            return null
    }
    
    var stack = Stack[TreeNode]()
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
     var node = new TreeNode(value, token)
     while (!stack.isEmpty && node.value <= stack.top.value) {
                node.left = stack.pop()
            }
     if (!stack.isEmpty) {
                stack.top.right = node
            }
     stack.push(node)
    }
    if (stack == null) {
            return null
    }
     var rst :TreeNode = stack.pop() 
        while (!stack.isEmpty) {
            rst = stack.pop()
        }
        return rst
  }
  
  def getWeight(base: Int, s: String) : Int = {  
    if (s == "+" || s == "-") {
            return base + 1
        }
    if (s == "*" || s == "/") {
        return base + 2
    }
    return Integer.MAX_VALUE   
  }
  
  def getSymbols(rst: ArrayBuffer[String], node: TreeNode) : ArrayBuffer[String] = {
    if (node == null) {
    		return rst
    	}
  	if (node.left != null) {
  	    getSymbols(rst, node.left)
  	}
  	if (node.right != null) {
  		  getSymbols(rst, node.right)
  	}
  	
     node.symbol match{
      case "(" =>     
      case ")" =>     
      case _ => rst += node.symbol 
    }
    return rst
  } 
  
    def getResultJson(error :Boolean, result :Double, message :String) : JsValue = {      
      val rst = error match{
        case true => Json.obj(
        "error" -> true,
        "message" -> message)  
        case false => Json.obj(
        "error" -> false,
        "result" -> result)  
      }
      return rst
  }
}