package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;

/**
 */
public class CDMethodInvokeExpression extends CDExpression {

    private final CDExpression _object;
    private final String _methodName;
    private final ArrayList _args = new ArrayList();

    // use the invoke method on CDExpression.
    protected CDMethodInvokeExpression(CDExpression obj, String methodname) {
        _object = obj;
        _methodName = methodname;
    }
    public CDMethodInvokeExpression(String methodname) {
        _object = null;
        _methodName = methodname;
    }
    
    /** Adds an argument to this invocation. */
    public CDMethodInvokeExpression arg( CDExpression arg ) {
        _args.add(arg);
        return this;
    }
    /** Adds arguments to this invocation. */
    public CDMethodInvokeExpression args( CDExpression[] args ) {
        for( int i=0; i<args.length; i++ )
            arg( args[i] );
        return this;
    }
    
    public CDStatement asStatement() {
        return new CDExpressionStatement(this);
    }
    
    public void express(CDFormatter f) throws IOException {
        
        if(_object != null) {
            f.express(_object).p('.');
        }
        f.p(_methodName).p('(');
        
        boolean first = true;
        for (Iterator itr = _args.iterator(); itr.hasNext();) {
            
            if(!first)  f.p(',');
            first = false;
            
            f.express( (CDExpression) itr.next() );
        }
        
        f.p(')');
    }

}
