package relaxngcc.codedom;

import java.io.IOException;
import java.io.Writer;

/**
 * Operator factory
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class CDOp {

    public static final int EQ = 1;
    public static final int STREQ = 2;
    public static final int AND = 3;
    public static final int OR = 4;
    
    private static class BinaryOperator extends CDExpression {
        private int _type;
        private CDExpression _left;
        private CDExpression _right;
    
        protected BinaryOperator(int type, CDExpression left, CDExpression right) {
            _type = type;
            _left = left;
            _right = right;
        }
        
        public void express(CDFormatter f) throws IOException {
            if(_type==STREQ) {
                f.express(_left).p('.').p("equals").p('(').express(_right).p(')');
            } else {
                //TODO: eliminate excessive brackets
                f.p('(');
                _left.express(f);
                switch(_type) {
                    case AND:
                        f.p(" && ");
                        break;
                    case OR:
                        f.p(" || ");
                        break;
                    case EQ:
                        f.p(" == ");
                        break;
                }
                _right.express(f);
                f.p(')');
            }
        }
    }
    
    /** Object identity equality operator. */
    public static CDExpression EQ(CDExpression left, CDExpression right) {
        return new BinaryOperator(EQ, left, right);
    }
    /** String value equality operator. */
    public static CDExpression STREQ(CDExpression left, CDExpression right) {
        return new BinaryOperator(STREQ, left, right);
    }
    /** Logical and operator. */
    public static CDExpression AND(CDExpression left, CDExpression right) {
        return new BinaryOperator(AND, left, right);
    }
    /** Logical or operator. */
    public static CDExpression OR(CDExpression left, CDExpression right) {
        return new BinaryOperator(OR, left, right);
    }
    /** logical not operator */
    public static CDExpression NOT(final CDExpression exp) {
        return new CDExpression() {
            public void express(CDFormatter f) throws IOException {
                f.p('(').p('!').express(exp).p(')');
            }
        };
    }


}
