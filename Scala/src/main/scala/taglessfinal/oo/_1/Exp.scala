package taglessfinal.oo._1;

/*
    The expression problem:
    We cannot extend existing code in two dimensions: operations and interpreters

    In the OO approach we can easily add a new operation (Mul), but adding a new interpreter (view())
    would break existing code, because the new method view must be added to the interface Exp and all classes
    implementing this interfaces.

    In the FP approach adding an interpreter is straight forward, but adding a new operation would break
    existing code.
 */
trait Exp {
  def eval: Int
}

case class Lit(n: Int) extends Exp {
  override def eval: Int = n
}

case class Add(x: Exp, y: Exp) extends Exp {
  override def eval: Int = x.eval + y.eval
}

case class Mul(x: Exp, y: Exp) extends Exp {
  override def eval: Int = x.eval * y.eval
}

object Main extends App {

  val e1 = Add(Lit(1), Add(Lit(2), Lit(3)))
  val e2 = Mul(e1, Lit(2))

  println(e1.eval)
  println(e2.eval)
}

