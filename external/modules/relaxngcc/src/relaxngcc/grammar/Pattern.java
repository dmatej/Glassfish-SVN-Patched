package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class Pattern {
    public abstract Object apply( PatternFunction f );
}
