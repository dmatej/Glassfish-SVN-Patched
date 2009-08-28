package relaxngcc.grammar;

import relaxngcc.codedom.CDExpression;
import relaxngcc.codedom.CDLanguageSpecificString;
import relaxngcc.codedom.CDType;

/**
 * NGCC Parameter for scope definitions.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class NGCCDefineParam {
    
    public NGCCDefineParam( String _className, String _access,
        String _returnType, String _returnValue, String _params ) {
        
        if(_returnType==null)   _returnType=_className;
        if(_returnValue==null)  _returnValue="this";
        if(_access==null)       _access="";
        if(_className==null)    _className = "RelaxNGCC_Result";
        
        this.className = _className;
        this.access = _access;
        this.returnType = new CDType(_returnType);
        this.returnValue = new CDLanguageSpecificString(_returnValue);
        this.params = _params;
    }
    
    /** Class name to generate. */
    public final String className;
    
    /** Access modifiers. */
    public final String access;
    
    /** Return-type from this state. */
    public final CDType returnType;
    
    /** Return-value from this state. */
    public final CDExpression returnValue;
    
    /** Additional parameters to this state */
    public final String params;
}

