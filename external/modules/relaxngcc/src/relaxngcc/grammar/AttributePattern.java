package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class AttributePattern extends MarkupPattern {
    public AttributePattern(Locator sloc, Locator eloc, NameClass _name, Pattern _body) {
        
        super(sloc,eloc,_name, _body);
    }
    
    public Object apply( PatternFunction f ) {
        return f.attribute(this);
    }
}
