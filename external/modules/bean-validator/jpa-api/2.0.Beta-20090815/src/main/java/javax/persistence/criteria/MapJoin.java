// $Id: MapJoin.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import java.util.Map;
import javax.persistence.metamodel.MapAttribute;

/**
 * The MapJoin interface is the type of the result of
 * joining to a collection over an association or element
 * collection that has been specified as a java.util.Map.
 *
 * @param <Z> The source type of the join
 * @param <K> The type of the target Map key
 * @param <V> The type of the target Map value
 */
public interface MapJoin<Z, K, V>
		extends PluralJoin<Z, Map<K, V>, V> {

    /**
     * Return the metamodel representation for the map attribute.
     * @return metamodel type representing the Map that is
     *         the target of the join
     */
    MapAttribute<? super Z, K, V> getModel();

    /**
     * Specify an inner join over the map key.
     * @return result of joining over the map key
     */
    Join<Map<K, V>, K> joinKey();

    /**
     * Specify a join over the map key using the given
     * join type.
     * @param jt  join type
     * @return result of joining over the map key
     */
    Join<Map<K, V>, K> joinKey(JoinType jt);

    /**
     * Return a path expression that corresponds to the map key.
     * @return path corresponding to map key
     */
    Path<K> key();

    /**
     * Return a path expression that corresponds to the map value.
     * This method is for stylistic use only: it just returns this.
     * @return path corresponding to the map value
     */
    Path<V> value();

    /**
     * Return an expression that corresponds to the map entry.
     * @return expression corresponding to the map entry
     */
    Expression<Map.Entry<K, V>> entry();
}

