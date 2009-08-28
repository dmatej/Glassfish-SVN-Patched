package relaxngcc.grammar;

import org.xml.sax.Locator;

/**
 * A pattern defined by a &lt;define> and &lt;start>.
 * 
 * A start pattern is represented by <code>name==null</code>.
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class Scope extends Pattern {
    public Scope( String _name ) {
        this.name = _name;
    }
    
    /**
     * Name of this pattern.
     * Copied from the name attribute of the &lt;define> element.
     * <p>
     * For the start pattern, this field is null.
     */
    public final String name;
    
    private NGCCDefineParam param;
    public void setParam(NGCCDefineParam p) { this.param=p; }
    /** NGCC parameters associated to this scope. */
    public NGCCDefineParam getParam() { return param; }

    
    /** Body of this definition. */
    private Pattern p;
    public Pattern getPattern() { return p; }
    
    /** Code specified via &lt;cc:java-import> statements. */
    private String imports;
    public void appendImport( String code ) {
        if(imports==null)   imports=code;
        else                imports+=code;
    }
    public String getImport() { return imports; }

    /** Code specified via &lt;cc:java-body> statements. */
    private String body;
    public void appendBody( String code ) {
        if(body==null)   body=code;
        else                body+=code;
    }
    public String getBody() { return body; }
    
    /** Incorporates the newly discovered &lt;define>. */
    public void append( Pattern pattern, String method ) {
        // TODO: handle @combine
        if(method!=null)
            throw new UnsupportedOperationException();
        this.p = pattern;
    }

    public Object apply( PatternFunction f ) {
        return f.scope(this);
    }
}
