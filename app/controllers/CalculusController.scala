package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import play.api.Logger
import java.util.Base64
import java.nio.charset.StandardCharsets
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import play.api.libs.json.Json

@Singleton
class CalculusController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {  
  
  private val logger = Logger(getClass)
  
  def read(query: String): Action[AnyContent] = {
    Action { implicit request =>
     val decodedExpression = getUTF8Query(query)
     var cleanedExpression = cleanQuery(decodedExpression)
     var RPNExpression = convertToRPNExpression(cleanedExpression)
     var result = calculate(RPNExpression)
     Ok(Json.obj(
        "error" -> false,
        "result" -> result,
        "message" -> "It's okay."
      ))
    }
  }
  
  def getUTF8Query(query: String) : String = {
    val base64RegEx = """^([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==)$""".r   
    val is_base64 = base64RegEx.pattern.matcher(query).matches
    val string = is_base64 match{
        case true => return decodeBase64toUTF8(query)
        case false => query
    }      
    return query
  }
  
  def cleanQuery(expression: String) : String = {    
    val nospaces = expression.replaceAll("\\s+", "") //clean all empty spaces between letters    
    val onlyallowedsymbols = nospaces.replaceAll("[^\\(\\)\\*\\+\\-\\d/\\s]", "") //clean all chars except +-*/()    
    val listofsymbols = onlyallowedsymbols.split("(?<=[-+*/()])|(?=[-+*/()])") //separate digits from symbols    
    var result :String = "" //build a clear string with spaces between every digit/symbol
    for(x <- listofsymbols){
      result = result + x + " "             
    }
    return result
  }
  
  def decodeBase64toUTF8(expression: String) : String = {
    val decoded = Base64.getDecoder.decode(expression)
    val stringQuery = new String(decoded, StandardCharsets.UTF_8)
    return stringQuery;
  }
  
  def convertToRPNExpression(expression: String) : String = {      
    val rst = new ListBuffer[String]()
    val finalthing = new ListBuffer[String]()
    for(x <- expression.split(" ")){
      rst += x      
    }
    var root :TreeNode = convertExpressionToReversePolishNotation(rst)
    val res = getSymbols(finalthing, root) 
    var stringlist :String = ""
    for(x <- res){
      stringlist = stringlist + x + " "             
    }
    return stringlist.replaceAll("[()]","")
  }
  
  def calculate(expression: String) : Double = {
    var stack = Stack[Double]()
    for(token <- expression.split("\\s+")){
      logger.trace("calculate: hi it's here " + token)
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
        case _ =>  stack.push(token.toDouble)
      }    
    } 
  
    return stack.pop()
  }
  
  class TreeNode (v: Int, s: String) {
    var value: Int = v
    var symbol: String = s
    var left: TreeNode = null
    var right: TreeNode = null
  }
  
  def convertExpressionToReversePolishNotation(expression: ListBuffer[String]) : TreeNode = {
    if (expression == null || expression.length == 0) {
            return null;
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
        return rst;
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
  
  def getSymbols(rst: ListBuffer[String], node: TreeNode) : ListBuffer[String] = {
    if (node == null) {
    		return rst
    	}
  	if (node.left != null) {
  	    getSymbols(rst, node.left)
  	}
  	if (node.right != null) {
  		  getSymbols(rst, node.right)
  	}
    rst += node.symbol
    return rst
  } 
  
}
