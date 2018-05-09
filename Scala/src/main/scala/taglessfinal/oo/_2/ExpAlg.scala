package taglessfinal.oo._2

/*
    An object algebra allows us to extend in 2 dimensions without loosing type safety.
    It is basically the same as tagless final with FP.

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

trait Eval {
  // operation 'eval'
  def eval: Int
}

class EvalExp extends ExpAlg[Eval] {

  override def lit(n: Int): Eval = new Eval() {
    override def eval: Int = n
  }

  override def add(x: Eval, y: Eval): Eval = new Eval() {
    override def eval: Int = x.eval + y.eval
  }
}

// --- In order to add a new operation 'mul' we are
// --- extending the Algebra ExpAlg with multiplication
//
trait MulAlg[T] extends ExpAlg[T] {
  // action 'mul'
  def mul(x: T, y: T): T
}

class EvalMul extends EvalExp with MulAlg[Eval] {
  override def mul(x: Eval, y: Eval): Eval = new Eval() {
    override def eval: Int = x.eval * y.eval
  }
}

// --- Adding a new interpreter 'view'
//
trait View {
  // operation 'view'
  def view: String
}

class ViewExp extends ExpAlg[View] {

  override def lit(n: Int): View = new View() {
    override def view: String = n.toString
  }

  override def add(x: View, y: View): View = new View() {
    override def view: String = "(" + x.view + " + " + y.view + ")"
  }
}

class ViewMul extends ViewExp with MulAlg[View] {

  override def mul(x: View, y: View): View = new View() {
    override def view: String = "(" + x.view + " * " + y.view + ")"
  }
}


object Main extends App {

  private def e1[T](f: ExpAlg[T]): T = f.add(f.lit(1), f.add(f.lit(2), f.lit(3)))

  private def e2[T](f: MulAlg[T]): T = f.mul(f.lit(4), f.add(f.lit(5), f.lit(6)))


  val v1: Int = e1(new EvalExp).eval
  println("" + v1)

  val v2: Int = e2(new EvalMul).eval
  println("" + v2)

  val s1: String = e1(new ViewExp).view
  println("" + s1)

  val s2: String = e2(new ViewMul).view
  println("" + s2)
}