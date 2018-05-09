package jtaglessfinal.oo._1;

/*
    The expression problem:
    We cannot extend existing code in two dimensions: operations and interpreters

    In the OO approach we can easily add a new operation (Mul), but adding a new interpreter (view())
    would break existing code, because the new method view must be added to the interface Exp and all classes
    implementing this interfaces.

    In the FP approach adding an interpreter is straight forward, but adding a new operation would break
    existing code.
 */
interface Exp {
    int eval();
}

class Lit implements Exp {

    final int n;

    Lit(int n) {
        this.n = n;
    }

    @Override
    public int eval() {
        return n;
    }
}

class Add implements Exp {

    final Exp x, y;

    Add(Exp x, Exp y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int eval() {
        return x.eval() + y.eval();
    }
}

class Mul implements Exp {

    final Exp x, y;

    Mul(Exp x, Exp y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int eval() {
        return x.eval() * y.eval();
    }
}

public class Main {

    private static void println(String s) {
        System.out.println(s);
    }

    public static void main(String[] args) {
        Exp e1 = new Add (new Lit(1), new Add(new Lit (2), new Lit(3)));
        Exp e2 = new Mul(e1, new Lit(2));

        println("" + e1.eval());
        println("" + e2.eval());
    }
}
