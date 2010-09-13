// $Id: Type.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type Type represent persistent object
 * or attribute types.
 *
 * @param <X>  The type of the represented object or attribute
 */
public interface Type<X> {

	public static enum PersistenceType {
		ENTITY, EMBEDDABLE, MAPPED_SUPERCLASS, BASIC
	}

	/**
	 * Return the persistence type.
	 *
	 * @return persistence type
	 */
	PersistenceType getPersistenceType();

	/**
	 * Return the represented Java type.
	 *
	 * @return Java type
	 */
	Class<X> getJavaType();
}
