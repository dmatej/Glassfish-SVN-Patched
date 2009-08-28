package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 */
public class CDType {
    
    public static final CDType VOID    = new CDType("void");
    public static final CDType INTEGER = new CDType("int");
    public static final CDType BOOLEAN = new CDType("boolean");
    public static final CDType STRING  = new CDType("String");
    
    public CDType( String name ) {
        _name = name;
    }
    
    private final String _name;
    
    /** Gets the display name of the type. */
    public String getName() { return _name; }
    
    
    /** Creates a new instance of this type. */
    public CDObjectCreateExpression _new() {
        return new CDObjectCreateExpression(this);
    }
    
    /** Gets the array type of this type. */
    public CDType array() {
        final CDType baseType = this;
        return new CDType(_name+"[]") {
            public boolean isArray() { return true; }
        };
    }

    /** Prints the type name. */
    public void writeType( CDFormatter f ) throws IOException {
        f.p(_name);
    }
    
    /** Returns true if this is an array type. */
    public boolean isArray() { return false; }
}
