// $Id: Path.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.criteria;

import javax.persistence.metamodel.Bindable;
import javax.persistence.metamodel.MapAttribute;
import javax.persistence.metamodel.PluralAttribute;
import javax.persistence.metamodel.SingularAttribute;

/**
 * Represents a simple or compound attribute path from a
 * bound type or collection, and is a "primitive" expression.
 * @param <X>  Type referenced by the path
 */
public interface Path<X> extends Expression<X> {

    /**
     * Return the bindable object that corresponds to the
     * path expression.
     * @return bindable object corresponding to the path
     */
    Bindable<X> getModel();

    /**
     *  Return the parent "node" in the path or null if no parent.
     *  @return parent
     */
    Path<?> getParentPath();

    /**
     *  Return the path corresponding to the referenced
     *  single-valued attribute.
     *  @param attribute single-valued attribute
     *  @return path corresponding to the referenced attribute
     */
    <Y> Path<Y> get(SingularAttribute<? super X, Y> attribute);

    /**
     *  Return the path corresponding to the referenced
     *  collection-valued attribute.
     *  @param collection collection-valued attribute
     *  @return expression corresponding to the referenced attribute
     */
    <E, C extends java.util.Collection<E>> Expression<C> get(PluralAttribute<X, C, E> collection);

    /**
     *  Return the path corresponding to the referenced
     *  map-valued attribute.
     *  @param map map-valued attribute
     *  @return expression corresponding to the referenced attribute
     */
    <K, V, M extends java.util.Map<K, V>> Expression<M> get(MapAttribute<X, K, V> map);

    /**
     *  Return an expression corresponding to the type of the path.
     *  @return expression corresponding to the type of the path
     */
    Expression<Class<? extends X>> type();


    //String-based:

    /**
     *  Return the path corresponding to the referenced
     *  attribute.
     *  @param attributeName  name of the attribute
     *  @return path corresponding to the referenced attribute
     *  @throws IllegalStateException if invoked on a path that
     *          corresponds to a basic type
     *  @throws IllegalArgumentException if attribute of the given
     *          name does not otherwise exist
     */
    <Y> Path<Y> get(String attributeName);
}
