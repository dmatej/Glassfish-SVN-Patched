package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ListPattern extends Pattern {
    public ListPattern( Locator loc, Pattern _p, String _alias ) {
        this.p = _p;
        this.alias = _alias;
        this.locator = loc;
    }
    
    public final Pattern p;
    public final String alias;
    public final Locator locator;
    
    public Object apply( PatternFunction f ) {
        return f.list(this);
    }
}
