package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 */
public class CDAssignStatement implements CDStatement {

    private CDExpression _destination;
    private CDExpression _source;

    // use CDBlock to create one
    CDAssignStatement(CDExpression destination, CDExpression source) {
        _destination = destination;
        _source = source;
    }
    

    public void state(CDFormatter f) throws IOException {
        f.express(_destination).p('=').express(_source).eos().nl();
    }
}
