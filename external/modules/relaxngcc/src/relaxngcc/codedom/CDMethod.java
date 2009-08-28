package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Iterator;

/**
 */
public class CDMethod {

    private CDLanguageSpecificString _preModifier;
    private CDType _returnType;
    private String _name;
    private CDLanguageSpecificString _postModifier;
    /** Parameters to this method. List of VariableDeclaration. */
    private final ArrayList _params = new ArrayList();
    
    private final CDBlock _body = new CDBlock();
    
    public CDMethod(CDLanguageSpecificString forwardspecifier,
        CDType returntype, String name,
        CDLanguageSpecificString backwardspecifier ) {
        
        _preModifier = forwardspecifier;
        _returnType = returntype;
        _name = name;
        _postModifier = backwardspecifier;
    }
    
    /**
     * Adds a new parameter to this method and returns a reference
     * to it.
     */
    public CDVariable param( CDType type, String name ) {
        CDVariable v = new CDVariable(null,type,name,null);
        _params.add(v);
        return v;
    }
    
    /** Gets a reference to the method body. */
    public CDBlock body() { return _body; }

    public void writeTo( CDFormatter f ) throws IOException {

        if(_preModifier!=null)
            f.write(_preModifier);
        
        if(_returnType!=null)
            f.type(_returnType);
        
        f.p(_name).p('(');
        
        boolean first=true;
        for (Iterator itr = _params.iterator(); itr.hasNext();) {
            if(!first)  f.p(',');
            first = false;
            
            f.declare((CDVariable) itr.next());
        }
        f.p(')');
        
        if(_postModifier!=null)
            f.write(_postModifier);
        
        f.state(_body).nl();
        
    }

}
