package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 * Base class of ElementPattern and AttributePattern
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class MarkupPattern extends Pattern {
    public MarkupPattern( Locator sloc, Locator eloc, NameClass _name, Pattern _body ) {
        this.name = _name;
        this.body = _body;
        this.startLocator = sloc;
        this.endLocator = eloc;
        
        if(_name==null || _body==null)
            throw new IllegalArgumentException();
    }
    
    public final Locator startLocator;
    public final Locator endLocator;
    public final NameClass name;
    public final Pattern body;
}
