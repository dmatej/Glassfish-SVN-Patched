package relaxngcc.grammar;

import org.xml.sax.Locator;

import relaxngcc.MetaDataType;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DataPattern extends Pattern {
    
    public DataPattern( Locator loc, MetaDataType _type, String _alias ) {
        this.alias = _alias;
        this.type = _type;
        this.locator = loc;
    }
    
    public final MetaDataType type;
    public final String alias;
    public final Locator locator;
    
    public Object apply( PatternFunction f ) {
        return f.data(this);
    }
}
