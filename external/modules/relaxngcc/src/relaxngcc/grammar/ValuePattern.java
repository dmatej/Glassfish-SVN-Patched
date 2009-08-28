package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ValuePattern extends Pattern {
    public ValuePattern( Locator loc, String _value, String _alias ) {
        this.value = _value;
        this.alias = _alias;
        this.locator = loc;
    }
    
    public final String value;
    public final String alias;
    public final Locator locator;
    
    public Object apply( PatternFunction f ) {
        return f.value(this);
    }
}
