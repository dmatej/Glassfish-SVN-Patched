package relaxngcc.grammar;

/**
 * Parameters attached to a &lt;ref> pattern.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NGCCCallParam {
    public NGCCCallParam( String _withParams, String _alias ) {
        this.withParams = _withParams;
        this.alias = _alias;
    }
    
    private final String alias;
    
    private String withParams;
    
    public String getAlias() {
        return alias;
    }

    public String getWithParams() {
        return withParams;
    }

    public void setWithParams(String withParams) {
        this.withParams = withParams;
    }
}

