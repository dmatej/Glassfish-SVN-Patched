package relaxngcc.grammar;

import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

/**
 * Set of {@link Scope}s.
 * 
 * <p>
 * Grammar as {@link Scope} holds information about the start pattern.
 * This allows us to easily recognize Grammar in the parsed tree.
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public final class Grammar extends Scope {
    
    public Grammar( Grammar _parent ) {
        super(null);
        this.parent = _parent;
    }
    
    /** Parent {@link Grammar} object, if any. Otherwise null. */
    public final Grammar parent;
    
    /** from pattern name to Scope. */
    private final Map patterns = new Hashtable();
    
    /** Gets the Scope object or return null. */
    public Scope get( String name ) {
        if(name==null)  throw new IllegalArgumentException();
        return (Scope)patterns.get(name);
    }
    
    public Scope getOrCreate( String name ) {
        Scope s = get(name);
        if(s==null)
            patterns.put(name,s=new Scope(name));
        return s;
    }
    
    /** Iterates all the named {@link Scope}s in this grammar. */
    public Iterator iterateScopes() {
        return patterns.values().iterator();
    }
}
