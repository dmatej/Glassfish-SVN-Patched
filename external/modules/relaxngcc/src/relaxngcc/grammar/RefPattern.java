package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class RefPattern extends Pattern {
    public RefPattern( Locator loc, Scope _target, NGCCCallParam _param ) {
        this.target = _target;
        this.param = _param;
        this.locator = loc;
    }
    
    public final Locator locator;
    public final Scope target;
    public final NGCCCallParam param;

    public Object apply( PatternFunction f ) {
        return f.ref(this);
    }
}
