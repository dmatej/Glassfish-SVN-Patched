// $Id: Bindable.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * Instances of the type Bindable represent object or attribute types
 * that can be bound into a Path.
 *
 * @param <T>  The type of the represented object or attribute
 */
public interface Bindable<T> {

	public static enum BindableType {
		SINGULAR_ATTRIBUTE, PLURAL_ATTRIBUTE, ENTITY_TYPE
	}

	/**
	 * Return the bindable type of the represented object.
	 *
	 * @return bindable type
	 */
	BindableType getBindableType();

	/**
	 * Return the Java type of the represented object.
	 * If the bindable type of the object is PLURAL_ATTRIBUTE,
	 * the Java element type is returned. If the bindable type is
	 * SINGULAR_ATTRIBUTE or ENTITY_TYPE, the Java type of the
	 * represented entity or attribute is returned.
	 *
	 * @return Java type
	 */
	Class<T> getBindableJavaType();
}
