package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NotAllowedPattern extends Pattern {
    public NotAllowedPattern() {}
    
    public Object apply( PatternFunction f ) {
        return f.notAllowed(this);
    }
}
