package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class InterleavePattern extends BinaryPattern {
    public InterleavePattern( Pattern p1, Pattern p2 ) {
        super(p1,p2);
    }
    
    public Object apply( PatternFunction f ) {
        return f.interleave(this);
    }
}
