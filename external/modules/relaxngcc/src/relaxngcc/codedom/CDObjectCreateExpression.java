package relaxngcc.codedom;
import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;

/**
 */
public class CDObjectCreateExpression extends CDExpression implements CDStatement {
    
    /** [left].new [type]([args]); */
    private final CDExpression _left;
    private final CDType _type;
    private final ArrayList _args = new ArrayList();
    
    /** use CDType._new */
    CDObjectCreateExpression(CDType type ) {
        this(null,type);
    }
    
    CDObjectCreateExpression(CDExpression left,CDType type) {
        _left = left;
        _type = type;
    }
    
    public CDObjectCreateExpression arg( CDExpression arg ) {
        _args.add(arg);
        return this;
    }

    public void express(CDFormatter f) throws IOException {
        if(_left!=null)
            f.express(_left).p('.');
        
        if(_type.isArray()) {
            f.p("new").type(_type).p('{');
        } else {
            f.p("new").type(_type).p('(');
        }
            
        boolean first = true;
        for (Iterator itr = _args.iterator(); itr.hasNext();) {
            if(!first)  f.p(',');
            first = false;
            
            CDExpression arg = (CDExpression) itr.next();
            f.express(arg);
        }
            
        if(_type.isArray())
            f.p('}');
        else
            f.p(')');
    }

    public void state(CDFormatter f) throws IOException {
        express(f);
        f.eos().nl();
    }
    
}
