package relaxngcc.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Iterator that returns a subset of another iterator
 * by filtering out some of the elements.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class SelectiveIterator implements Iterator {
    public SelectiveIterator( Iterator base ) {
        _base = base;
    }
    /** base iterator to filter. */
    private final Iterator _base;

    /** return false to skip this object. */
    protected abstract boolean filter( Object o );
            
    private Object _next;
    /** Finds the next object to return, if any. */
    private void findNext() {
        if(_next != null)  return;
        
        while(_base.hasNext()) {
            _next = _base.next();
            if(filter(_next))
                return;  // this is fine.
        }
        _next = null;  // not found
    }
    
    public boolean hasNext() {
        findNext();
        return _next!=null;
    }
    public Object next() {
        findNext();
        if(_next==null)  throw new NoSuchElementException();
        
        Object r = _next;
        _next=null;
        return r;
    }
    public void remove() { throw new UnsupportedOperationException(); }
    
}

