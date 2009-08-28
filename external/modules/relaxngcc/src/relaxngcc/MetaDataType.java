/*
 * MetaDataType.java
 *
 * Created on 2001/08/04, 21:52
 */

package relaxngcc;
import java.io.PrintStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;


/**
 * MetaDataType has the ability to generate a code that verifies data types described in original grammar.
 * At the present, only XML Schema Part2 is supported as the data type library for RelaxNGCC.
 */
public class MetaDataType {
    public MetaDataType( String name ) { _name = name; }
    
    /** Type name. */
    public final String _name;
}
