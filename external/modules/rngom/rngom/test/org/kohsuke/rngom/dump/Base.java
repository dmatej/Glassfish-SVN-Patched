package org.kohsuke.rngom.dump;

/**
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
abstract class Base {
    protected final Factory factory;
    protected final Printer printer;
    protected final int id;
    
    public Base( Factory f, Printer p, int id ) {
        this.factory = f;
        this.printer = p;
        this.id = id;
    }
    
    protected Printer out( String name ) {
        return printer.object(toString()).name(name);
    }
    
    protected abstract String prefix();
    
    public String toString() {
        return prefix()+id;
    }
}
