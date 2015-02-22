import scala.collection.mutable
import scala.util.parsing.combinator.{JavaTokenParsers, RegexParsers}

abstract class ExpressionSymbol {
  def eval(setVal:Option[Int] = None)(implicit valMap:mutable.HashMap[String,Int]):Option[Int]
}

case class Variable(name:String) extends ExpressionSymbol {
  override def eval(setVal:Option[Int])(implicit valMap:mutable.HashMap[String,Int]):Option[Int] = {
    valMap.get(name)
  }
}
case class Const(value: Int) extends ExpressionSymbol {
  override def eval(setVal:Option[Int])(implicit valMap: mutable.HashMap[String, Int]): Option[Int] = Some(value)
}
case class Assignment(lhs:ExpressionSymbol,rhs:ExpressionSymbol) extends ExpressionSymbol {
  override def eval(setVal:Option[Int])(implicit valMap: mutable.HashMap[String, Int]): Option[Int] = {
    rhs.eval() match {
      case None => throw new IllegalArgumentException()
      case  someVal  => {
       lhs match {
         case varb : Variable => {
           valMap.put(varb.name,someVal.get)
         }
         case _ => throw new IllegalArgumentException(s"LHS must be a variable  and not ${lhs.toString}")
       }
        someVal
    }
    }

  }
}

case class Add(x: ExpressionSymbol, y: ExpressionSymbol) extends ExpressionSymbol {
  override def eval(setVal: Option[Int])(implicit valMap: mutable.HashMap[String, Int]): Option[Int] = {
    x.eval() match  {
      case None => {
        y.eval()
      }
      case someVal => {
        y.eval() match {
          case None => someVal
          case yEval => Some(someVal.get + yEval.get)
        }
      }
    }
  }
}
case class Mul(x: ExpressionSymbol, y: ExpressionSymbol) extends ExpressionSymbol {
  override def eval(setVal: Option[Int])(implicit valMap: mutable.HashMap[String, Int]): Option[Int] = {
    x.eval() match  {
      case None => {
        y.eval()
      }
      case someVal => {
        y.eval() match {
          case None => someVal
          case yEval => Some(someVal.get * yEval.get)
        }
      }
    }
  }
}

object ExpressionParser extends JavaTokenParsers {

  def variable: Parser[ExpressionSymbol] = ident  ^^ {
    case str => Variable(str)
  }
  def compExp : Parser[ExpressionSymbol] = (variable ~ "=" ~ operation1) ^^ {
    case v ~ str ~ oper => Assignment(v, oper)
  }
  def expression = operation1
  def operation1: Parser[ExpressionSymbol] = operation2 ~ rep("+" ~ operation2) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "+" ~ y) => Add(x, y)
    }
  }
  def operation2: Parser[ExpressionSymbol] = operand ~ rep("*" ~ operand) ^^ {
    case op ~ list => list.foldLeft(op) {
      case (x, "*" ~ y) => Mul(x, y)
    }
  }
  def operand: Parser[ExpressionSymbol] = (constant | variable)
  /*def variable: Parser[ExpressionSymbol] = "x" ^^ { _ => X }*/
  def constant: Parser[ExpressionSymbol] = """-?\d+""".r ^^ { s => Const(s.toInt) }

  def apply(input: String)(implicit vals : scala.collection.mutable.HashMap[String,Int] ): Option[ExpressionSymbol] = parseAll(compExp, input) match {
    case Success(result, _) => {
      println(result.eval())
      Some(result)
    }
    case NoSuccess(_, _) => None
  }



}
object Evaluate extends App{
  override def main(args:Array[String]):Unit = {
    implicit val vals = scala.collection.mutable.HashMap[String,Int]()
    val exp = ExpressionParser("a = 10")
    val exp1 = ExpressionParser("b = a + 8")
    val exp2 = ExpressionParser("c = a + b * a")
    println(exp)
    println(exp1)
    println(exp2)
    println(10+18 * 10)
  }
}