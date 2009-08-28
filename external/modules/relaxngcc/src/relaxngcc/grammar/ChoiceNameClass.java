package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ChoiceNameClass extends NameClass {
    public ChoiceNameClass( NameClass _nc1, NameClass _nc2 ) {
        this.nc1 = _nc1;
        this.nc2 = _nc2;
    }
    
    public final NameClass nc1;
    public final NameClass nc2;

    public Object apply(NameClassFunction f) {
        return f.choice(nc1,nc2);
    }
    
    public String toString() {
        return nc1.toString()+'|'+nc2.toString();
    }
}
