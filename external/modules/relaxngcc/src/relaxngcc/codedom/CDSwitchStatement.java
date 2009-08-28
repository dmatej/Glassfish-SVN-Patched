package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.Vector;

/**
 */
public class CDSwitchStatement implements CDStatement {

    private class Block {
        CDConstant _expr;
        CDBlock _statements;
        
        Block(CDConstant e, CDBlock s) {
            _expr = e;
            _statements = s;
        }
    }
    
    private CDExpression _checkValue;
    private Vector _blocks;
    private CDBlock _defaultBlock;
    
    public CDSwitchStatement(CDExpression expr) {
        _checkValue = expr;
        _blocks = new Vector();
        _defaultBlock = null;
    }
    public void addCase(CDConstant expr, CDBlock statements) {
        if(_defaultBlock!=null) throw new IllegalStateException("this SwitchStatement is closed already");
        _blocks.add(new Block(expr, statements));
    }
//    public void setDefaultCase(CDBlock statements) {
//        if(_DefaultBlock!=null) throw new IllegalStateException("this SwitchStatement is closed already");
//        _DefaultBlock = statements;
//    }
    public CDBlock defaultCase() {
        if(_defaultBlock==null) _defaultBlock = new CDBlock();
        return _defaultBlock;
    }

    public void state(CDFormatter f) throws IOException {

        f.p("switch").p('(').express(_checkValue).p(')').p('{').nl();
        
        for(int i=0; i<_blocks.size(); i++) {
            final Block block = (Block)_blocks.get(i);
            
            f.p("case").express(block._expr).p(':').nl();
            
            f.in();
            f.state(block._statements);
            f.p("break").eos().nl();
            f.out();
        }
        
        if(_defaultBlock!=null) {
            f.p("default:").nl();
            
            f.in();
            f.state(_defaultBlock);
            f.p("break").eos().nl();
            f.out();
        }
        
        f.p('}').nl();
    }

}
