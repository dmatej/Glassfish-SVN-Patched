package relaxngcc.grammar;

/**
 * visits a Pattern tree.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class PatternWalker implements PatternFunction {

    public Object empty(EmptyPattern p) {
        return null;
    }

    public Object notAllowed(NotAllowedPattern p) {
        return null;
    }

    public Object group(GroupPattern p) {
        p.p1.apply(this);
        p.p2.apply(this);
        return null;
    }

    public Object interleave(InterleavePattern p) {
        p.p1.apply(this);
        p.p2.apply(this);
        return null;
    }

    public Object choice(ChoicePattern p) {
        p.p1.apply(this);
        p.p2.apply(this);
        return null;
    }

    public Object oneOrMore(OneOrMorePattern p) {
        return p.p.apply(this);
    }

    public Object element(ElementPattern p) {
        return p.body.apply(this);
    }

    public Object attribute(AttributePattern p) {
        return p.body.apply(this);
    }

    public Object data(DataPattern p) {
        return null;
    }

    public Object value(ValuePattern p) {
        return null;
    }

    public Object list(ListPattern p) {
        return p.p.apply(this);
    }

    public Object ref(RefPattern p) {
        return p.target.apply(this);
    }

    public Object scope(Scope s) {
        return s.getPattern().apply(this);
    }

    public Object javaBlock(JavaBlock p) {
        return null;
    }

}
