package relaxngcc.util;

import java.util.Iterator;

/**
 * Iterator that walks over two other iterators.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ConcatIterator implements Iterator {
    public ConcatIterator( Iterator first, Iterator second ) {
        _first = first;
        _second = second;
    }
    
    private Iterator _first, _second;
    
    public boolean hasNext() { return _first.hasNext() || _second.hasNext(); }
    public Object next() {
        if(_first.hasNext()) return _first.next();
        else                return _second.next();
    }
    public void remove() { throw new UnsupportedOperationException(); }
}
