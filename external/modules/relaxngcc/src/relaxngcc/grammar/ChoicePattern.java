package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ChoicePattern extends BinaryPattern {
    public ChoicePattern( Pattern p1, Pattern p2 ) {
        super(p1,p2);
    }
    
    public Object apply( PatternFunction f ) {
        return f.choice(this);
    }
}
