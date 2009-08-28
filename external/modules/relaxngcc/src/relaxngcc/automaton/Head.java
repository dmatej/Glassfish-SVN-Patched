package relaxngcc.automaton;

/**
 * Place holder for the description of HEAD(x).
 * 
 * <p>
 * HEAD(x) is defined for both states and transitions.
 * 
 * <p>
 * Informally, HEAD(t) is a set of all non-reference alphabets
 * that can cause a valid transition by 't'.
 * Similarly, HEAD(s) is a set of all non-reference alphabets
 * that can cause a valid transition from 's'.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class Head {
    
    /** A special alphabet that means "all other alphabets". */
    public static final Object EVERYTHING_ELSE = new Object();
}

