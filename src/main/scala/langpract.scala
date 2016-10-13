import nl.typeset.sonofjson._
import nl.typeset
import scala.io.Source
import java.io._


abstract class exprC
  case class numC(n: Int) extends exprC
  case class idC(i: String) extends exprC
  case class plusC (l : Tuple2[exprC, exprC]) extends exprC
  case class multC (l : Tuple2[exprC, exprC]) extends exprC
  case class letC (l : Tuple3[String, exprC, exprC]) extends exprC
  case class letrecC (l : Tuple3[String, exprC, exprC]) extends exprC
  case class appC (l : Tuple2[exprC, exprC]) extends exprC
  case class lamC (l : Tuple2[String, exprC]) extends exprC
  case class trueC (t : Boolean) extends exprC
  case class falseC (f : Boolean) extends exprC
  case class consC (l : Tuple2[exprC, exprC]) extends exprC
  case class restC (r : exprC) extends exprC
  case class listC (l : List[exprC]) extends exprC
  case class emptyC (l : List[Nothing]) extends exprC
  case class firstC (e : exprC) extends exprC
  case class emptyListC (e : exprC) extends exprC
  case class eqC (e : Tuple2[exprC, exprC]) extends exprC
  case class ifC (l : Tuple3[exprC, exprC, exprC]) extends exprC


abstract class Value
  case class numV(n: Int) extends Value
  case class closV(c: Tuple3[String, exprC, Environment]) extends Value
  case class boolV(b: Boolean) extends Value
  case class listV(l: List[Value]) extends Value
  case class emptylistV() extends Value


class Binding(s : String, v : Value) {
  var sym: String = s;
  var value: Value = v;
}

class Environment(b : List[Binding]) {
  var binds: List[Binding] = b;
}

object MyInterp{
  def main(args: Array[String]): Unit = {
    val file = "foo.json"
    var k : String = " ";
    try{
    	val resource : InputStream = this.getClass.getResourceAsStream("/" + file);
    	k = scala.io.Source.fromInputStream(resource).mkString;
    	val l = resource.close();
	} catch {
		case e: FileNotFoundException =>
			println("Could not open file");
		case e: IOException =>
			println("IOException");
	}
	println("Parsing: ");
	println(k);
	val input : exprC = Parser.parsejsV(k);
    val dummy : Binding = new Binding("dummy", new numV(-1));
    val mtEnv = new Environment(List(dummy));
   
    println("Interpreting: " + input);
  	val v : Value = Parser.interp(input, mtEnv);
  	v match {
  		case numV (n) =>
  			println("Number: " + n);
  		case closV (c) =>
  			println("ClosV: " + c._1 + " , " + c._2 + " " + c._3.binds);
  		case boolV (b) =>
  			println("Boolean: " + b);
  		case listV (l) =>
  			println("List: " + l);
  		case emptylistV () =>
  			println("Empty");
  	}
  }

}



object Parser {
  def parsejsV(js : String) : exprC = {
    val dummy : Binding = new Binding("dummy", new numV(-1));
    val mtEnv = new Environment(List(dummy));
    val rawJ : String = js;
    val n = parse(rawJ);
    var parsed : exprC = new numC(-1);
    try{
    	parsed = parseExp(n.prog);
    } catch{
    	case ex: IOException => {
    		println("Error invalid input");
    	}
    }
    return parsed;
  }

  def parseExp(exp : JValue) : exprC = {
    var expr : String = exp.expr.as[String];
    expr match {
      case "number" =>
        new numC(exp.number.as[Int]);
      case "id" =>
        new idC(exp.id.as[String]);
      case "add" =>
        var l = parseExp(exp.left);
        var r = parseExp(exp.right);
        new plusC(new Tuple2(l, r));
      case "mult" =>
        var l = parseExp(exp.left);
        var r = parseExp(exp.right);
        new multC(new Tuple2(l, r));
      case "let" =>
        val s : String = exp.id.as[String];
        val r : exprC = parseExp(exp.`val`);
        val l : exprC = parseExp(exp.body);
        new letC(new Tuple3(s, r, l));
      case "letrec" =>
        val s : String = exp.id.as[String];
        new letrecC(new Tuple3(s, parseExp(exp.`val`), parseExp(exp.body)));
      case "application" =>
        new appC(new Tuple2(parseExp(exp.func), parseExp(exp.arg)));
      case "lambda" =>
        val s : String = exp.param.as[String];
        new lamC(new Tuple2(s, parseExp(exp.body)));
      case "boolean" =>
        var b : Boolean = exp.bool.as[Boolean];
        if(b){
          new trueC(true);
        }else{
          new falseC(false);
        }
      case "cons" =>
        new consC(Tuple2(parseExp(exp.first), parseExp(exp.rest)));
      case "rest" =>
        new restC(parseExp(exp.list));
      case "list" =>
        var l : List[exprC] = List[exprC]();
        l = parseExp(exp.first) :: l;
        l = parseExp(exp.rest) :: l;
        new listC(l);
      case "empty" =>
        var l : List[Nothing] = List[Nothing]();
        new emptyC(l);
      case "first" =>
        new firstC(parseExp(exp.list));
      case "empty?" =>
        new emptyListC(parseExp(exp.list));
      case "equal" =>
        var l = parseExp(exp.left);
        var r = parseExp(exp.right);
        new eqC(new Tuple2(l, r));
      case "if" =>
      	var cond = parseExp(exp.cond);
      	var tval = parseExp(exp.then);
      	var eval = parseExp(exp.`else`);
      	new ifC(new Tuple3(cond, tval, eval));
      case _ =>
      	throw new IOException("Error: invalid JSON input");
    }
  }


  def interp(expression: exprC, environment: Environment): Value = {
    val ex = expression;
    val env = environment;
    ex match {
      case numC(n) => new numV(n)
      case idC(i) => 
      	var v : Value = new numV(-1);
      	if(env.binds.size == 1){
      		throw new Exception("Empty environment");
      	}else {
      		v = lookup(i, env);
      		if(v == numV(-1)){
      			throw new Exception("idc not found");
      		}
      	}
      	return v;
      case plusC (l) =>
        val left = interp(l._1, env)
        left match {
          case numV (n) =>
            val leftVal = n
            val right = interp(l._2, env)
            right match {
              case numV (n)  => new numV (leftVal + n);
              case _ =>
            	throw new Exception("Not a number");
            }
            case _ =>
            	throw new Exception("Not a number");
        }
      case multC (l) =>
        val left = interp(l._1, env)
        left match {
          case numV (n) =>
            val leftVal = n
            val right = interp(l._2, env)
            right match {
              case numV (n)  => new numV (leftVal * n)
              case _ =>
              	throw new Exception("Not a number");
            }
            case _ =>
            	throw new Exception("Not a number");
        }

      case letC (l)  =>
        val b : Binding = new Binding(l._1, interp(l._2, env));
        env.binds = b :: env.binds;
        interp(l._3 , env);

      case letrecC (l) =>
        var a : Value = numV(-1);
        var b : Binding = new Binding(l._1, a);
        env.binds = b :: env.binds;
        a = interp(l._2, env);
        b = new Binding(l._1, a);
        env.binds = b :: env.binds;
        interp(l._3, env);

      case appC (l) =>
          var fv : Value = interp(l._1, env);
          fv match {
            case closV (c) =>
              var b : Binding = new Binding(c._1, interp(l._2, c._3));
              env.binds = b :: env.binds;
              interp(c._2, env);
            case _ =>
              throw new Exception("not a function");
          }
      case lamC (l) =>
          new closV(Tuple3(l._1, l._2, env));
      case trueC (t)=>
          new boolV(true);
      case falseC (f) =>
          new boolV(false);
      case consC (l) =>
      		val first = interp(l._1, env);
          l._2 match {
          case emptyC (l) =>
          	new listV(List(first));
          case listC (li) =>
            new listV(first :: (li).map(interp(_, env)));
          case consC (k) =>
          	var re = List(first, interp(k._1,env));
          	val conslist : Value = interp(k._2, env);
          	conslist match {
          		case listV(l) =>
          		re = re ::: l;
          		case _ =>
            		throw new Exception("Not a list");
          	}
			new listV(re);
          case _ =>
            	throw new Exception("Not a list");
          }
       case restC (r) =>
          r match {
            case listC (l) =>
              var con : List[exprC] = l.slice(1, l.size);
              var li : listC = new listC(con);
              var v : Value = interp(li, env);
              v;
            case consC (l) =>
            	// return the second element of cons
            	interp(l._2, env);
            case _ =>
            	throw new Exception("Not a list");
          }
        case listC (l) =>
          new listV(l.map(interp(_, env)));
        case emptyC (l) =>
          new emptylistV();
        case firstC (e) =>
          var l : Value = interp(e, env);
          l match {
            case listV (li) =>
              li(1);
          }
        case emptyListC (e) =>
          var l : Value = interp(e, env);
          l match {
            case emptylistV () =>
              new boolV(true);
            case _ =>
              new boolV(false);
          }
        case eqC (e) =>
          var a : Value = interp(e._1, env);
          var b : Value = interp(e._2, env);
          if(a == b){
            new boolV(true);
          }else{
            new boolV(false);
          }
         case ifC (l) =>
         	var cond = interp(l._1, env);
         	cond match {
         		case boolV (b)=> 
         			if(b){
         				interp(l._2, env);
         			} else {
         				interp(l._3, env);
         			}
         		case _ =>
         			throw new Exception("not a boolean");
         	}
         	
    }
  }


  def lookup(f : String, env : Environment): Value ={
    if(env.binds.isEmpty){
     	throw new Exception("Empty environment");
    } else{
      var i = 0;
      for(i <- 0 to env.binds.size){
        if(env.binds(i).sym == f){
          return env.binds(i).value;
        }
      }
      throw new Exception("Unbound identifier");
    }
  }
}
