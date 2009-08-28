package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class GroupPattern extends BinaryPattern {
    public GroupPattern( Pattern p1, Pattern p2 ) {
        super(p1,p2);
    }
    
    public Object apply( PatternFunction f ) {
        return f.group(this);
    }
}
