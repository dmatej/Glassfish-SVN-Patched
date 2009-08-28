package relaxngcc.builder;

import java.text.MessageFormat;

import relaxngcc.codedom.CDOp;
import relaxngcc.codedom.CDConstant;
import relaxngcc.codedom.CDExpression;
import relaxngcc.codedom.CDVariable;
import relaxngcc.grammar.NameClass;
import relaxngcc.grammar.NameClassFunction;

/**
 * Generates a clause that tests the membership of a NameClass.
 * 
 * <p>
 * This function returns {@link CDExpression}.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NameTestBuilder implements NameClassFunction {
    
    public NameTestBuilder( CDExpression uriVar, CDExpression localNameVar ) {
        _$uriVar = uriVar;
        _$localNameVar = localNameVar;
    }
    
    public static CDExpression build( NameClass nc, CDExpression uri, CDExpression local ) {
        return (CDExpression)nc.apply(new NameTestBuilder(uri,local));
    }
    
    private final CDExpression _$uriVar;
    private final CDExpression _$localNameVar;
    
    public Object choice(NameClass nc1, NameClass nc2) {
        return CDOp.OR(
            (CDExpression)nc1.apply(this),
            (CDExpression)nc2.apply(this));
    }

    public Object nsName(String ns, NameClass except) {
        CDExpression exp = _$uriVar.invoke("equals").arg(new CDConstant(ns));
        
        if(except!=null)
            exp = CDOp.AND( exp,
                ((CDExpression)except.apply(this)).not() );
        
        return exp;
    }

    public Object anyName(NameClass except) {
        if(except==null)
            return new CDConstant(true);
        else
            return ((CDExpression)except.apply(this)).not();
    }

    public Object name(String ns, String local) {
        return CDOp.AND(
            CDOp.STREQ( _$uriVar, new CDConstant(ns) ),
            CDOp.STREQ( _$localNameVar, new CDConstant(local)) );
    }

}

