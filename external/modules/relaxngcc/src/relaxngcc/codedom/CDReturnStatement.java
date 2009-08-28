package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

public class CDReturnStatement implements CDStatement {

    private CDExpression _expression;
    
    /** use CDBlock to create an instance. */
    CDReturnStatement(CDExpression expr) {
        _expression = expr;
    }

    public void state(CDFormatter f) throws IOException {
        f.p("return").p('(').express(_expression).p(')').eos().nl();
    }

}
