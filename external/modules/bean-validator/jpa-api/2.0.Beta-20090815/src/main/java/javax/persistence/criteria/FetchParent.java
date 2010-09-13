// $Id: FetchParent.java 17233 2009-08-05 18:10:41Z steve.ebersole@jboss.com $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.metamodel.PluralAttribute;
import javax.persistence.metamodel.SingularAttribute;

/**
 * Represents an element of the from clause which may
 * function as the parent of Fetches.
 *
 * @param <Z> The type of the fetch source
 * @param <X> The type of the fetched attribute/association
 */
public interface FetchParent<Z, X> {

    /**
     *  Return the fetch joins that have been made from this type.
     *  @return fetch joins made from this type
     */
    java.util.Set<Fetch<X, ?>> getFetches();

    /**
     *  Fetch join to the specified single-valued attribute
     *  using an inner join.
     *  @param attribute  target of the join
     *  @return the resulting fetch join
     */
    <Y> Fetch<X, Y> fetch(SingularAttribute<? super X, Y> attribute);

    /**
     *  Fetch join to the specified single-valued attribute using
     *  the given join type.
     *  @param attribute  target of the join
     *  @param jt  join type
     *  @return the resulting fetch join
     */
    <Y> Fetch<X, Y> fetch(SingularAttribute<? super X, Y> attribute, JoinType jt);

    /**
     *  Fetch join to the specified collection-valued attribute
     *  using an inner join.
     *  @param attribute  target of the join
     *  @return the resulting join
     */
    <Y> Fetch<X, Y> fetch(PluralAttribute<? super X, ?, Y> attribute);

    /**
     *  Fetch join to the specified collection-valued attribute
     *  using the given join type.
     *  @param attribute  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <Y> Fetch<X, Y> fetch(PluralAttribute<? super X, ?, Y> attribute, JoinType jt);


    //String-based:

    /**
     * Fetch join to the specified attribute using an inner join.
	 *
	 * @param <X> The type of the source of the fetch; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link FetchParent}; the expectation is that the two types match.
	 * @param <Y> The type of the fetched attribute/association
     * @param attributeName  name of the attribute for the
     *         target of the join
     * @return the resulting fetch join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> Fetch<X, Y> fetch(String attributeName);

    /**
	 * Fetch join to the specified attribute using the given
     * join type.
	 *
	 * @param <X> The type of the source of the fetch; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link FetchParent}; the expectation is that the two types match.
	 * @param <Y> The type of the fetched attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     *  @param jt  join type
     *  @return the resulting fetch join
     *  @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> Fetch<X, Y> fetch(String attributeName, JoinType jt);
}
