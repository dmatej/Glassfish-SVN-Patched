// $Id:$
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type SingularAttribute represents persistent
 * single-valued properties or fields.
 *
 * @param <X> The type containing the represented attribute
 * @param <T> The type of the represented attribute
 */
public interface SingularAttribute<X, T>
		extends Attribute<X, T>, Bindable<T> {

	/**
	 * Is the attribute an id attribute.
	 *
	 * @return boolean indicating whether or not attribute is an id
	 */
	boolean isId();

	/**
	 * Is the attribute a version attribute.
	 *
	 * @return boolean indicating whether or not attribute is
	 *         a version attribute
	 */
	boolean isVersion();

	/**
	 * Can the attribute be null.
	 *
	 * @return boolean indicating whether or not the attribute can
	 *         be null
	 */
	boolean isOptional();

	/**
	 * Return the type that represents the type of the attribute.
	 *
	 * @return type of attribute
	 */
	Type<T> getType();
}
