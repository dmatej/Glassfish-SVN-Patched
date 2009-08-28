package relaxngcc.builder;

import relaxngcc.grammar.ChoiceNameClass;
import relaxngcc.grammar.ElementPattern;
import relaxngcc.grammar.NameClass;
import relaxngcc.grammar.Pattern;
import relaxngcc.grammar.PatternWalker;

/**
 * Visits a pattern tree and
 * computes the name class that represents all the possible
 * element names in this pattern
 * 
 * <p>
 * Use the static "collect" method.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
class ElementNameCollector extends PatternWalker {
    public static NameClass collect( Pattern p ) {
        ElementNameCollector enc = new ElementNameCollector();
        p.apply(enc);
        // TODO: simplify this name class
        return enc._nameClass;
    }
    
    private NameClass _nameClass = null;
    
    public Object element(ElementPattern p) {
        if(_nameClass==null)
            _nameClass=p.name;
        else
            _nameClass = new ChoiceNameClass(_nameClass,p.name);
        return null;
    }
}
