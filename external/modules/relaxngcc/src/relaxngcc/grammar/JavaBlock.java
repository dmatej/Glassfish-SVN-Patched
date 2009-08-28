package relaxngcc.grammar;

/**
 * Represents &lt;cc:java> block.
 * This class derives Pattern so that it can be mixed with
 * other patterns. However, only GroupPattern is allowed to
 * have this Pattern as its child.
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class JavaBlock extends Pattern {
    
    public JavaBlock( String _code ) {
        this.code = _code;
    }

    public Object apply( PatternFunction f ) {
        return f.javaBlock(this);
    }
    
    /** code fragment. */
    public final String code;
    
    public boolean isPattern() { return false; }
    public Pattern asPattern()  { return null; }
    
    public boolean isJavaBlock()    { return true; }
    public JavaBlock asJavaBlock()  { return this; }
}
