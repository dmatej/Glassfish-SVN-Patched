package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public interface NameClassFunction {
    Object choice(NameClass nc1, NameClass nc2);
    Object nsName(String ns, NameClass except);
    Object anyName(NameClass except);
    Object name(String ns, String local);
}