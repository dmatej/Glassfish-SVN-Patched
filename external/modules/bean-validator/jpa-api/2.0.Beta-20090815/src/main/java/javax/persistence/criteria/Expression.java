// $Id: Expression.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.Collection;

/**
 * Type for query expressions.
 * @param <T> the type of the expression
 */
public interface Expression<T> extends Selection<T> {

    /**
     *  Apply  a predicate to test whether the expression is null.
     *  @return predicate testing whether the expression is null
     */
    Predicate isNull();

    /**
     *  Apply a predicate to test whether the expression is not null.
     *  @return predicate testing whether the expression is not null.
     */
    Predicate isNotNull();

    /**
     * Apply a predicate to test whether the expression is a member
     * of the argument list.
     * @param values
     * @return predicate testing for membership in the list
     */
    Predicate in(Object... values);

    /**
     * Apply a predicate to test whether the expression is a member
     * of the argument list.
     * @param values
     * @return predicate testing for membership
     */
    Predicate in(Expression<?>... values);

    /**
     * Apply a predicate to test whether the expression is a member
     * of the collection.
     * @param values collection
     * @return predicate testing for membership
     */
    Predicate in(Collection<?> values);

    /**
     * Apply a predicate to test whether the expression is a member
     * of the collection.
     * @param values expression corresponding to collection
     * @return predicate testing for membership
     */
    Predicate in(Expression<Collection<?>> values);

    /**
     * Perform a typecast upon the expression.
     * Warning: may result in a runtime failure.
     * @param type
     * @return expression
     */
    <X> Expression<X> as(Class<X> type);
}
