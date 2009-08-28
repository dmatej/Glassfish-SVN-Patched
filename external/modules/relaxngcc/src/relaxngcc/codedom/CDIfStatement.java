package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.Vector;

/**
 */
public class CDIfStatement implements CDStatement {
    
    private CDBlock _thenBlock;
    private CDBlock _elseBlock;
    private CDExpression _testExp;
    
    public CDIfStatement(CDExpression expr) {
        if(expr==null) throw new IllegalArgumentException("expr is null");
        _testExp=expr;
    }
    
    public void setThenBlock(CDBlock b) { _thenBlock=b; }
    public CDBlock _then() {
        if(_thenBlock==null)    _thenBlock = new CDBlock();
        return _thenBlock;
    }
    
    public void setElseBlock(CDBlock b) { _elseBlock=b; }
    public CDBlock _else() {
        if(_elseBlock==null)    _elseBlock = new CDBlock();
        return _elseBlock;
    }
    
    public void state( CDFormatter f ) throws IOException {

        f.p("if").p('(').express(_testExp).p(')');
        
        if( _thenBlock!=null)
            f.state(_thenBlock);
        else
            f.p(';');
        
        if( _elseBlock!=null) {
            f.p("else").state(_elseBlock);
        }
    }

}
