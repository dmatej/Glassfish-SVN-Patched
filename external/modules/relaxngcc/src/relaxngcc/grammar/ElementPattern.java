package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ElementPattern extends MarkupPattern {
    public ElementPattern(Locator sloc, Locator eloc, NameClass _name, Pattern _body) {
        super(sloc, eloc, _name, _body);
    }
    public Object apply( PatternFunction f ) {
        return f.element(this);
    }
}
