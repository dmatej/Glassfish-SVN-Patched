package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 */
public class CDConstant extends CDExpression {

    /** Type of this constant. */
    private final CDType _type;
    private int _intVal;
    private boolean _booleanVal;
    private String _stringVal;
    
    public CDConstant(int value) {
        _type = CDType.INTEGER;
        _intVal = value;
    }
    public CDConstant(boolean value) {
        _type = CDType.BOOLEAN;
        _booleanVal = value;
    }
    public CDConstant(String value) {
        _type = CDType.STRING;
        _stringVal = value;
    }
    
    
    private static class Atom extends CDExpression {
        private Atom( String token ) { _token = token; }
        private final String _token;
        public void express(CDFormatter f) throws IOException {
            f.p(_token);
        }
    }
    

    public static final CDExpression NULL = new Atom("null");
    public static final CDExpression THIS = new Atom("this");
    public static final CDExpression SUPER = new Atom("super");
    
    public void express(CDFormatter f) throws IOException {
        if(_type==CDType.INTEGER) {
               f.p(Integer.toString(_intVal));
               return;
        }
        if(_type==CDType.BOOLEAN) {
               f.p(_booleanVal? "true" : "false");
            return;
        }
        if(_type==CDType.STRING) {
               f.p('"'+_stringVal+'"');
            return;
        }
        throw new IllegalStateException();
    }

}
