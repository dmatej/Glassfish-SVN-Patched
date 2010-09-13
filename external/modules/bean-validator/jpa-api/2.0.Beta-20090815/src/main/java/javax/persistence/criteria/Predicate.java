// $Id: Predicate.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.List;

/**
 * The type of a simple or compound predicate: a conjunction or
 * disjunction of restrictions.
 * A simple predicate is considered to be a conjunction with a
 * single conjunct.
 */
public interface Predicate extends Expression<Boolean> {

	public static enum BooleanOperator {
		AND, OR
	}

    /**
     * Return the boolean operator for the predicate.
     * If the predicate is simple, this is AND.
     * @return boolean operator for the predicate
     */
    BooleanOperator getOperator();

    /**
     * Has negation been applied to the predicate.
     * @return boolean indicating if the predicate has been negated
     */
    boolean isNegated();

    /**
     * Return the top-level conjuncts or disjuncts of the predicate.
     * @return list of boolean expressions forming the predicate
     */
    List<Expression<Boolean>> getExpressions();

    /**
     * Create a negation of the predicate.
     * @return negated predicate
     */
    Predicate negate();
}