// $Id: Metamodel.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Provides access to the metamodel of persistent
 * entities in the persistence unit.
 */
public interface Metamodel {

	/**
	 * Return the metamodel entity type representing the entity.
	 *
	 * @param cls the type of the represented entity
	 *
	 * @return the metamodel entity type
	 *
	 * @throws IllegalArgumentException if not an entity
	 */
	<X> EntityType<X> entity(Class<X> cls);

	/**
	 * Return the metamodel managed type representing the
	 * entity, mapped superclass, or embeddable class.
	 *
	 * @param cls the type of the represented managed class
	 *
	 * @return the metamodel managed type
	 *
	 * @throws IllegalArgumentException if not a managed class
	 */
	<X> ManagedType<X> type(Class<X> cls);

	/**
	 * Return the metamodel embeddable type representing the
	 * embeddable class.
	 *
	 * @param cls the type of the represented embeddable class
	 *
	 * @return the metamodel embeddable type
	 *
	 * @throws IllegalArgumentException if not an embeddable class
	 */
	<X> EmbeddableType<X> embeddable(Class<X> cls);

	/**
	 * Return the metamodel managed types.
	 *
	 * @return the metamodel managed types
	 */
	java.util.Set<ManagedType<?>> getManagedTypes();

	/**
	 * Return the metamodel entity types.
	 *
	 * @return the metamodel entity types
	 */
	java.util.Set<EntityType<?>> getEntities();

	/**
	 * Return the metamodel embeddable types.
	 *
	 * @return the metamodel embeddable types
	 */
	java.util.Set<EmbeddableType<?>> getEmbeddables();
}

