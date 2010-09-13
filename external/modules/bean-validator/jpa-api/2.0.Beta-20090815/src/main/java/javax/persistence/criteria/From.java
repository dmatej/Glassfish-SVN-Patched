// $Id: From.java 17233 2009-08-05 18:10:41Z steve.ebersole@jboss.com $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.CollectionAttribute;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.MapAttribute;
import javax.persistence.metamodel.SetAttribute;

/**
 * Represents a bound type, usually an entity that appears in
 * the from clause, but may also be an embeddable belonging to
 * an entity in the from clause.
 * Serves as a factory for Joins of associations, embeddables and
 * collections belonging to the type, and for Paths of attributes
 * belonging to the type.
 * @param <Z>
 * @param <X>
 */
public interface From<Z, X> extends Path<X>, FetchParent<Z, X> {

    /**
     *  Return the joins that have been made from this type.
     *  @return joins made from this type
     */
    java.util.Set<Join<X, ?>> getJoins();

    /**
     *  Join to the specified single-valued attribute using an
     *  inner join.
     *  @param attribute  target of the join
     *  @return the resulting join
     */
    <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute);

    /**
     *  Join to the specified single-valued attribute using the
     *  given join type.
     *  @param attribute  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <Y> Join<X, Y> join(SingularAttribute<? super X, Y> attribute, JoinType jt);

    /**
     *  Join to the specified Collection-valued attribute using
     *  an inner join.
     *  @param collection  target of the join
     *  @return the resulting join
     */
    <Y> CollectionJoin<X, Y> join(CollectionAttribute<? super X, Y> collection);

    /**
     *  Join to the specified Set-valued attribute using an inner
     *  join.
     *  @param set  target of the join
     *  @return the resulting join
     */
    <Y> SetJoin<X, Y> join(SetAttribute<? super X, Y> set);

    /**
     *  Join to the specified List-valued attribute using an inner
     *  join.
     *  @param list  target of the join
     *  @return the resulting join
     */
    <Y> ListJoin<X, Y> join(ListAttribute<? super X, Y> list);

    /**
     *  Join to the specified Map-valued attribute using an inner
     *  join.
     *  @param map  target of the join
     *  @return the resulting join
     */
    <K, V> MapJoin<X, K, V> join(MapAttribute<? super X, K, V> map);

    /**
     *  Join to the specified Collection-valued attribute
     *  using the given join type.
     *  @param collection  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <Y> CollectionJoin<X, Y> join(CollectionAttribute<? super X, Y> collection, JoinType jt);

    /**
     *  Join to the specified Set-valued attribute using the given
     *  join type.
     *  @param set  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <Y> SetJoin<X, Y> join(SetAttribute<? super X, Y> set, JoinType jt);

    /**
     *  Join to the specified List-valued attribute using the
     *  given join type.
     *  @param list  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <Y> ListJoin<X, Y> join(ListAttribute<? super X, Y> list, JoinType jt);

    /**
     *  Join to the specified Map-valued attribute using the
     *  given join type.
     *  @param map  target of the join
     *  @param jt  join type
     *  @return the resulting join
     */
    <K, V> MapJoin<X, K, V> join(MapAttribute<? super X, K, V> map, JoinType jt);


    //String-based:

    /**
     * Join to the specified attribute using an inner join.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> Join<X, Y> join(String attributeName);

    /**
     *  Join to the specified Collection-valued attribute using an
     *  inner join.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> CollectionJoin<X, Y> joinCollection(String attributeName);

    /**
     *  Join to the specified Set-valued attribute using an inner
     *  join.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> SetJoin<X, Y> joinSet(String attributeName);

    /**
     *  Join to the specified List-valued attribute using an inner
     *  join.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> ListJoin<X, Y> joinList(String attributeName);

    /**
     *  Join to the specified Map-valued attribute using an inner
     *  join.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <K> The type of the key of the joined map.
	 * @param <V> The type of the value of the joined map.
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, K, V> MapJoin<X, K, V> joinMap(String attributeName);


    /**
     *  Join to the specified attribute using the given
	 *  join type.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @param jt  join type
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> Join<X, Y> join(String attributeName, JoinType jt);

    /**
     *  Join to the specified Collection-valued attribute using
     *  the given join type.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @param jt  join type
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> CollectionJoin<X, Y> joinCollection(String attributeName, JoinType jt);

    /**
     *  Join to the specified Set-valued attribute using
     *  the given join type.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @param jt  join type
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> SetJoin<X, Y> joinSet(String attributeName, JoinType jt);

    /**
     *  Join to the specified List-valued attribute using
     *  the given join type.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <Y> The type of the joined attribute/association
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @param jt  join type
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, Y> ListJoin<X, Y> joinList(String attributeName, JoinType jt);

    /**
     *  Join to the specified Map-valued attribute using
     *  the given join type.
	 *
	 * @param <X> The type of the source of the join; note that this method-local <X> hides the <X> defined as a type
	 * param to {@link From}; the expectation is that the two types match.
	 * @param <K> The type of the key of the joined map.
	 * @param <V> The type of the value of the joined map.
     * @param attributeName  name of the attribute for the
     *               target of the join
     * @param jt  join type
     * @return the resulting join
     * @throws IllegalArgumentException if attribute of the given
     *          name does not exist
     */
    <X, K, V> MapJoin<X, K, V> joinMap(String attributeName, JoinType jt);
}
