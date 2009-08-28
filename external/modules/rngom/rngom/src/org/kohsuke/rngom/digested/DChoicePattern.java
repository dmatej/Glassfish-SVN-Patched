package org.kohsuke.rngom.digested;

/**
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class DChoicePattern extends DContainerPattern {
    public boolean isNullable() {
        for( DPattern p=firstChild(); p!=null; p=p.next )
            if(p.isNullable())
                return true;
        return false;
    }
    public Object accept( DPatternVisitor visitor ) {
        return visitor.onChoice(this);
    }
}
