package relaxngcc.grammar;

/**
 *
 *
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public interface PatternFunction {
    Object empty(EmptyPattern p);
    Object notAllowed(NotAllowedPattern p);
    Object group(GroupPattern p);
    Object interleave(InterleavePattern p);
    Object choice(ChoicePattern p);
    Object oneOrMore(OneOrMorePattern p);
    Object element(ElementPattern p);
    Object attribute(AttributePattern p);
    Object data(DataPattern p);
    Object value(ValuePattern p);
    Object list(ListPattern p);
    Object ref(RefPattern p);
    // special patterns
    Object scope(Scope s);
    Object javaBlock(JavaBlock p);
}