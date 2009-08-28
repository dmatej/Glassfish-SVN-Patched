package relaxngcc.grammar;

import java.util.ArrayList;

/**
 * 
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class BinaryPattern extends Pattern {
    public BinaryPattern( Pattern _p1, Pattern _p2 ) {
        this.p1 = _p1;
        this.p2 = _p2;
    }
    
    /** Gets all the child patterns of this combinor. */
    public Pattern[] getChildPatterns() {
        ArrayList r = new ArrayList();
        getChildPatterns(r,this.getClass());
        return (Pattern[]) r.toArray(new Pattern[r.size()]);
    }
    
    private void getChildPatterns( ArrayList r, Class combinor ) {
        if(p1.getClass()==combinor)
            ((BinaryPattern)p1).getChildPatterns(r,combinor);
        else
            r.add(p1);
            
        if(p2.getClass()==combinor)
            ((BinaryPattern)p2).getChildPatterns(r,combinor);
        else
            r.add(p2);
    }
    
    public final Pattern p1;
    public final Pattern p2;
}
