package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.Vector;

/**
 * @author Daisuke OKAJIMA
 * 
 * simple collection of CDStatement objects
 */
public class CDBlock implements CDStatement {

    private final Vector _statements = new Vector();
    
    public CDBlock() {}
    public CDBlock(CDStatement s) {
        this();
        add(s);
    }
    
    public void add(CDStatement s) {
        if(s==null) throw new IllegalArgumentException("parameter is null");
        _statements.add(s);
    }
    public void add(CDBlock sv) {
        _statements.addAll(sv._statements);
    }
    
    public CDIfStatement _if( CDExpression exp ) {
        CDIfStatement s = new CDIfStatement(exp);
        _statements.add(s);
        return s;
    }
    
    /** Adds a new method invocation. */
    public CDMethodInvokeExpression invoke( CDExpression obj, String method ) {
        CDMethodInvokeExpression e = new CDMethodInvokeExpression(obj,method);
        add(e.asStatement());
        return e;
    }
    public CDMethodInvokeExpression invoke( String method ) {
        CDMethodInvokeExpression e = new CDMethodInvokeExpression(method);
        add(e.asStatement());
        return e;
    }
    /** Adds a new variable declaration. */
    public CDVariable decl(CDType type, String name) {
        CDVariable d = new CDVariable(null,type,name,null);
        add(d);
        return d;
    }
    public CDVariable decl(CDType type, String name, CDExpression init ) {
        CDVariable d = new CDVariable(null,type,name,init);
        add(d);
        return d;
    }
    /** Adds a new assignment. */
    public CDAssignStatement assign( CDExpression lhs, CDExpression rhs ) {
        CDAssignStatement a = new CDAssignStatement(lhs,rhs);
        add(a);
        return a;
    }
    /** Adds a new return statement. */
    public void _return( CDExpression val ) {
        add(new CDReturnStatement(val));
    }
    

    public int size() { return _statements.size(); }
    
    public void state(CDFormatter f) throws IOException {
        f.p('{').nl().in();
        
        for(int i=0; i<_statements.size(); i++)
            f.state( (CDStatement)_statements.get(i) );
        
        f.out().p('}').nl();
    }
}
