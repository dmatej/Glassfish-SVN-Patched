// $Id: Subquery.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

/**
 * The Subquery interface defines functionality that is
 * specific to subqueries.
 *
 * A subquery has an expression as its selection item.
 * @param <T> the type of the returned selection item.
 */
public interface Subquery<T> extends AbstractQuery<T>, Expression<T> {

    /**
     * Return the query of which this is a subquery.
     * @return the enclosing query or subquery
     */
    AbstractQuery<?> getParent();

    /**
     * Specify the item that is to be returned in the query result.
     * Replaces the previously specified selection, if any.
     * @param expression  expression specifying the item that
     *        is returned in the query result
     * @return the modified subquery
     */
    Subquery<T> select(Expression<T> expression);

    /**
     * Modify the subquery to restrict the result according
     * to the specified boolean expression.
     * Replaces the previously added restriction(s), if any.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param restriction  a simple or compound boolean expression
     * @return the modified subquery
     */
    Subquery<T> where(Expression<Boolean> restriction);

    /**
     * Modify the subquery to restrict the result according
     * to the conjunction of the specified restriction predicates.
     * Replaces the previously added restriction(s), if any.
     * If no restrictions are specified, any previously added
     * restrictions are simply removed.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param restrictions  zero or more restriction predicates
     * @return the modified subquery
     */
    Subquery<T> where(Predicate... restrictions);

    /**
     * Specify the expressions that are used to form groups over
     * the subquery results.
     * Replaces the previous specified grouping expressions, if any.
     * If no grouping expressions are specified, any previously
     * added grouping expressions are simply removed.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param grouping  zero or more grouping expressions
     * @return the modified subquery
     */
    Subquery<T> groupBy(Expression<?>... grouping);

    /**
     * Specify a restriction over the groups of the subquery.
     * Replaces the previous having restriction(s), if any.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param restriction  a simple or compound boolean expression
     * @return the modified subquery
     */
    Subquery<T> having(Expression<Boolean> restriction);

    /**
     * Specify restrictions over the groups of the subquery
     * according the conjunction of the specified restriction
     * predicates.
     * Replaces the previously added restriction(s), if any.
     * If no restrictions are specified, any previously added
     * restrictions are simply removed.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param restrictions  zero or more restriction predicates
     * @return the modified subquery
     */
    Subquery<T> having(Predicate... restrictions);

    /**
     * Specify whether duplicate query results will be eliminated.
     * A true value will cause duplicates to be eliminated.
     * A false value will cause duplicates to be retained.
     * If distinct has not been specified, duplicate results must
     * be retained.
     * This method only overrides the return type of the
     * corresponding AbstractQuery method.
     * @param distinct  boolean value specifying whether duplicate
     *        results must be eliminated from the subquery result or
     *        whether they must be retained
     * @return the modified subquery.
     */
    Subquery<T> distinct(boolean distinct);

    /**
     * Return the selection expression.
     * @return the item to be returned in the subquery result
     */
    Expression<T> getSelection();

    /**
     * Correlate a root of the enclosing query to a root of
     * the subquery and return the subquery root.
     * @param parentRoot  a root of the containing query
     * @return subquery root
     */
    <Y> Root<Y> correlate(Root<Y> parentRoot);

    /**
     * Correlate a join of the enclosing query to a join of
     * the subquery and return the subquery join.
     * @param parentJoin  join target of the containing query
     * @return subquery join
     */
    <X, Y> Join<X, Y> correlate(Join<X, Y> parentJoin);

    /**
     * Correlate a join to a Collection-valued association or
     * element collection in the enclosing query to a join of
     * the subquery and return the subquery join.
     * @param parentCollection  join target of the containing query
     * @return subquery join
     */
    <X, Y> CollectionJoin<X, Y> correlate(CollectionJoin<X, Y> parentCollection);

    /**
     * Correlate a join to a Set-valued association or
     * element collection in the enclosing query to a join of
     * the subquery and return the subquery join.
     * @param parentSet  join target of the containing query
     * @return subquery join
     */
    <X, Y> SetJoin<X, Y> correlate(SetJoin<X, Y> parentSet);

    /**
     * Correlate a join to a List-valued association or
     * element collection in the enclosing query to a join of
     * the subquery and return the subquery join.
     * @param parentList  join target of the containing query
     * @return subquery join
     */
    <X, Y> ListJoin<X, Y> correlate(ListJoin<X, Y> parentList);

    /**
     * Correlate a join to a Map-valued association or
     * element collection in the enclosing query to a join of
     * the subquery and return the subquery join.
     * @param parentMap  join target of the containing query
     * @return subquery join
     */
    <X, K, V> MapJoin<X, K, V> correlate(MapJoin<X, K, V> parentMap);

    /**
     *  Return the joins that have been made from the subquery.
     *  @return joins made from this type
     */
    java.util.Set<Join<?, ?>> getJoins();

}