package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class SimpleNameClass extends NameClass {
    public SimpleNameClass( String _nsUri, String _localName ) {
        this.nsUri = _nsUri;
        this.localName = _localName;
    }
    
    public final String nsUri;
    public final String localName;

    public Object apply(NameClassFunction f) {
        return f.name(nsUri,localName);
    }
    
    public String toString() {
        return localName;
    }
}
