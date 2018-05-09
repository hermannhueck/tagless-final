package taglessfinal.fp._1

/*
    The expression problem:
    We cannot extend existing code in two dimensions: operations and interpreters

    In the OO approach we can easily add a new operation (Mul), but adding a new interpreter (view())
    would break existing code, because the new method view must be added to the interface Exp and all classes
    implementing this interfaces.

    In the FP approach adding an interpreter is straight forward, but adding a new operation would break
    existing code.
 */
sealed trait Exp
final case class Lit(n: Int) extends Exp
final case class Add(x: Exp, y: Exp) extends Exp

object Exp {

  def eval(e: Exp): Int = e match {
    case Lit(n) => n
    case Add(x, y) => eval(x) + eval(y)
  }

  def view(e: Exp): String = e match {
    case Lit(n) => n.toString
    case Add(x, y) => s"(${view(x)} + ${view(y)})"
  }
}

object Main extends App {

  import Exp._

  val e1 = Add(Lit(1), Add(Lit(2), Lit(3)))

  println(eval(e1))
  println(view(e1))
}
