package relaxngcc.builder;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import relaxngcc.grammar.AttributePattern;
import relaxngcc.grammar.BinaryPattern;
import relaxngcc.grammar.ChoicePattern;
import relaxngcc.grammar.DataPattern;
import relaxngcc.grammar.ElementPattern;
import relaxngcc.grammar.EmptyPattern;
import relaxngcc.grammar.Grammar;
import relaxngcc.grammar.GroupPattern;
import relaxngcc.grammar.InterleavePattern;
import relaxngcc.grammar.JavaBlock;
import relaxngcc.grammar.ListPattern;
import relaxngcc.grammar.NotAllowedPattern;
import relaxngcc.grammar.OneOrMorePattern;
import relaxngcc.grammar.PatternFunction;
import relaxngcc.grammar.RefPattern;
import relaxngcc.grammar.Scope;
import relaxngcc.grammar.ValuePattern;

/**
 * Traverses a grammar and collects all {@link Scope} objects.
 * 
 * This object serves as a function and returns a {@link Set} that
 * contains all Scopes found in the specified pattern.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ScopeCollector implements PatternFunction {
    
    private final Set _scopes = new HashSet();
    
    // terminals
    public Object empty(EmptyPattern p) { return _scopes; }
    public Object notAllowed(NotAllowedPattern p) { return _scopes; }
    public Object data(DataPattern p) { return _scopes; }
    public Object value(ValuePattern p) { return _scopes; }
    public Object javaBlock(JavaBlock p) { return _scopes; }

    // binary ops.
    public Object group(GroupPattern p) { return binary(p); }
    public Object interleave(InterleavePattern p) { return binary(p); }
    public Object choice(ChoicePattern p) { return binary(p); }
    
    private Object binary(BinaryPattern p) {
        p.p1.apply(this);
        p.p2.apply(this);
        return _scopes;
    }

    // unary ops
    public Object oneOrMore(OneOrMorePattern p) { return p.p.apply(this); }
    public Object element(ElementPattern p) { return p.body.apply(this); }
    public Object attribute(AttributePattern p) { return p.body.apply(this); }
    public Object list(ListPattern p) { return p.p.apply(this); }
    public Object ref(RefPattern p) { return p.target.apply(this); }
    
    // this is the only place where things get a bit interesting
    public Object scope(Scope scope) {
        if(_scopes.add(scope)) {
            
            if(scope.getPattern()==null) {
                throw new IllegalArgumentException("Scope [" + scope.name + "] is not found.");
            }
            
            // if the content of this scope is not processed yet, then recurse.
            scope.getPattern().apply(this);
            
            if(scope instanceof Grammar) {
                // to generate classes for unreferenced patterns,
                // check all named patterns.
                Grammar g = (Grammar)scope;
                Iterator itr = g.iterateScopes();
                while(itr.hasNext())
                    scope( (Scope)itr.next() );
            }
        }
        return _scopes;
    }
}

