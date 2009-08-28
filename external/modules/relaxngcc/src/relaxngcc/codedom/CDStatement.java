package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 * Abstract statement of programming languages.
 * 
 * @author Daisuke OKAJIMA
 */
public interface CDStatement {
    /**
     * Prints itself as a statement.
     */
    void state( CDFormatter f ) throws IOException;
}
