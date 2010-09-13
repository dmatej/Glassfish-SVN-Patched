// $Id:$
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type EntityType represent entity types.
 *
 * @param <X> The represented entity type.
 */
public interface EntityType<X>
		extends IdentifiableType<X>, Bindable<X> {

	/**
	 * Return the entity name
	 *
	 * @return entity name
	 */
	String getName();
}