package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class OneOrMorePattern extends Pattern {
    public OneOrMorePattern( Pattern _p ) {
        this.p = _p;
    }
    
    public final Pattern p;
    
    public Object apply( PatternFunction f ) {
        return f.oneOrMore(this);
    }
}
