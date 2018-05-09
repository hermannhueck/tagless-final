package jtaglessfinal.oo._2;

/*
    An object algebra allows us to extend in 2 dimensions without loosing type safety.
    It is basically the same as tagless final with FP.

    A few words about the terminology. The abstract factory interface ExpAlg<T> is called an object algebra interface
    and concrete factories implementing it are called object algebras. The terminology and the approach in general are
    inspired by abstract algebra, in which algebraic structures (or simply algebras) are described by signatures. A signature
    acts as an interface that specifies the types of operations defined on the underlying set of an algebraic structure,
    and an algebra provides concrete definitions of those operations and is similar to a class implementing an interface.
 */
interface ExpAlg<T> {
    // actions 'lit' and 'add'
    T lit(int n);
    T add(T x, T y);
}

interface Eval {
    // operation 'eval'
    int eval();
}

class EvalExp implements ExpAlg<Eval> {

    @Override public Eval lit(int n) {
        return new Eval() {
            @Override public int eval() {
                return n;
            }
        };
    }

    @Override public Eval add(Eval x, Eval y) {
        return new Eval() {
            @Override public int eval() {
                return x.eval() + y.eval();
            }
        };
    }
}

// --- In order to add a new operation 'mul' we are
// --- extending the Algebra ExpAlg with multiplication
//
interface MulAlg<T> extends ExpAlg<T> {
    T mul(T x, T y);
}

class EvalMul extends EvalExp implements MulAlg<Eval> {
    @Override public Eval mul(final Eval x, final Eval y) {
        return new Eval() {
            @Override public int eval() {
                return x.eval() * y.eval();
            }
        };
    }
}

// --- Adding a new interpreter 'view'
//
interface View {
    String view();
}

class ViewExp implements ExpAlg<View> {
    @Override public View lit(final int n) {
        return new View() {
            @Override public String view() {
                return Integer.toString(n);
            }
        };
    }
    @Override public View add(final View x, final View y) {
        return new View() {
            @Override public String view() {
                return "(" + x.view() + " + " + y.view() + ")";
            }
        };
    }
}

class ViewMul extends ViewExp implements MulAlg<View> {
    @Override public View mul(final View x, final View y) {
        return new View() {
            @Override public String view() {
                return "(" + x.view() + " * " + y.view() + ")";
            }
        };
    }
}


public class Main {

    private static void println(String s) {
        System.out.println(s);
    }

    public static void main(String[] args) {
        new Main();
    }

    private <T> T e1(ExpAlg<T> f) {
        return f.add(
                f.lit(1),
                f.add(
                        f.lit(2),
                        f.lit(3)));
    }

    private <T> T e2(MulAlg<T> f) {
        return f.mul(
                f.lit(4),
                f.add(
                        f.lit(5),
                        f.lit(6)));
    }

    private Main() {

        int v1 = e1(new EvalExp()).eval();
        println("" + v1);

        int v2 = e2(new EvalMul()).eval();
        println("" + v2);

        String s1 = e1(new ViewExp()).view();
        println("" + s1);

        String s2 = e2(new ViewMul()).view();
        println("" + s2);
    }
}
