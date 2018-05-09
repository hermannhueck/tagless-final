package taglessfinal.fp._2

/*
    An object algebra allows us to extend in 2 dimensions without loosing type safety.
    It is basically the same as tagless final with FP.

    The code is heavily based on type classes what avoid wiring the traits/classes by inheritence.

    A few words about the terminology. The abstract factory interface ExpAlg<T> is called an object algebra interface
    and concrete factories implementing it are called object algebras. The terminology and the approach in general are
    inspired by abstract algebra, in which algebraic structures (or simply algebras) are described by signatures. A signature
    acts as an interface that specifies the types of operations defined on the underlying set of an algebraic structure,
    and an algebra provides concrete definitions of those operations and is similar to a class implementing an interface.
 */
trait ExpAlg[T] {
  // actions 'lit' and 'add'
  def lit (n: Int): T
  def add (x: T, y: T): T
}

case class Eval(eval: Int)

object Eval {

  implicit object ExpAlgEval extends ExpAlg[Eval] {
    override def lit(n: Int): Eval = Eval(n)
    override def add(x: Eval, y: Eval): Eval = Eval(x.eval + y.eval)
  }
}

// --- In order to add a new operation 'mul' we are
// --- extending the Algebra ExpAlg with multiplication
//
trait MulAlg[T] {
  // action 'mul'
  def mul (x: T, y: T): T
}

object MulAlg {

  implicit object MulAlgEval extends MulAlg[Eval] {
    override def mul(x: Eval, y: Eval): Eval = Eval(x.eval * y.eval)
  }
}

// --- Adding a new interpreter 'view'
//
case class View(view: String)

object View {

  implicit object ExpAlgView extends ExpAlg[View] with MulAlg[View] {
    override def lit(n: Int): View = new View(n.toString)
    override def add(x: View, y: View): View = new View("(" + x.view + " + " + y.view + ")")
    override def mul(x: View, y: View): View = new View("(" + x.view + " * " + y.view + ")")
  }
}


object Main extends App {

  private def e1[T](implicit fExp: ExpAlg[T]): T =
    fExp.add(fExp.lit(1), fExp.add(fExp.lit(2), fExp.lit(3)))

  private def e2[T](implicit fExp: ExpAlg[T], fMul: MulAlg[T]): T =
    fMul.mul(fExp.lit(4), fExp.add(fExp.lit(5), fExp.lit(6)))


  val v1: Int = e1[Eval].eval
  println("" + v1)

  val v2: Int = e2[Eval].eval
  println("" + v2)

  val s1: String = e1[View].view
  println("" + s1)

  val s2: String = e2[View].view
  println("" + s2)
}

