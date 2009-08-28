package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 * CDVariable. Can be used as an CDExpression to refer to this variable.
 * 
 */
public class CDVariable extends CDExpression implements CDStatement {

    private CDLanguageSpecificString _modifier;
    private CDType _type;
    private String _name;
    private CDExpression _initialValue;

    // not directly creatable. Use appropriate factory methods.
    CDVariable(
        CDLanguageSpecificString modifier,
        CDType type, String name, CDExpression initialvalue) {
            
        _modifier = modifier;
        _type = type;
        _name = name;
        _initialValue = initialvalue;
    }
    
    public String getName() { return _name; }
    
    public void express( CDFormatter f ) throws IOException {
        // as a reference
        f.p(_name);
    }

    public void declare( CDFormatter f ) throws IOException {
        // as mod type name [=init]

        if(_modifier != null)
            f.write(_modifier);
        
        f.type(_type).p(_name);
        
        if(_initialValue != null)
            f.p('=').express(_initialValue);
    }

    public void state( CDFormatter f ) throws IOException {
        // as a statement
        declare(f);
        f.eos().nl();
    }
}
