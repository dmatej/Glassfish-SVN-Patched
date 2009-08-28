package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class EmptyPattern extends Pattern {
    public EmptyPattern() {}
    
    public Object apply( PatternFunction f ) {
        return f.empty(this);
    }
}
