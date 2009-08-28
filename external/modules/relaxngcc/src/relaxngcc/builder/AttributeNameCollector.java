package relaxngcc.builder;

import relaxngcc.grammar.*;

/**
 * Visits a pattern tree and
 * computes the name class that represents all the possible
 * attribute names in this pattern
 * 
 * <p>
 * Use the static "collect" method.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
class AttributeNameCollector extends PatternWalker {
    public static NameClass collect( Pattern p ) {
        AttributeNameCollector anc = new AttributeNameCollector();
        p.apply(anc);
        // TODO: simplify this name class
        return anc._nameClass;
    }
    
    private NameClass _nameClass = null;
    
    public Object element(ElementPattern p) {
        return null;    // don't go inside elements.
    }
    
    public Object attribute(AttributePattern p) {
        if(_nameClass==null)
            _nameClass = p.name;
        else
            _nameClass = new ChoiceNameClass(_nameClass, p.name);
        return null;
    }
}
