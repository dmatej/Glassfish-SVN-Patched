package relaxngcc.grammar;

/**
 * 
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NsNameClass extends NameClass {
    public NsNameClass( String _uri, NameClass _except ) {
        this.uri = _uri;
        this.except = _except;
    }
    
    public final String uri;
    public final NameClass except;

    public Object apply(NameClassFunction f) {
        return f.nsName(uri,except);
    }
    
    public String toString() {
        return '{'+uri+'}'+(except!=null?'-'+except.toString():"");
    }
    
}
