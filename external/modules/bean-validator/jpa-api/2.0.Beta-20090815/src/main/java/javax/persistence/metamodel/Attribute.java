// $Id: Attribute.java 17038 2009-07-08 10:58:24Z epbernard $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence.metamodel;

/**
 * An attribute of a Java type
 *
 * @param <X> The represented type that contains the attribute
 * @param <Y> The type of the represented attribute
 */
public interface Attribute<X, Y> {

	public static enum PersistentAttributeType {
		MANY_TO_ONE, ONE_TO_ONE, BASIC, EMBEDDED,
		MANY_TO_MANY, ONE_TO_MANY, ELEMENT_COLLECTION
	}

	/**
	 * Return the name of the attribute.
	 *
	 * @return name
	 */
	String getName();

	/**
	 * Return the persistent attribute type for the attribute.
	 *
	 * @return persistent attribute type
	 */
	PersistentAttributeType getPersistentAttributeType();

	/**
	 * Return the managed type representing the type in which
	 * the attribute was declared.
	 *
	 * @return declaring type
	 */
	ManagedType<X> getDeclaringType();

	/**
	 * Return the Java type of the represented attribute.
	 *
	 * @return Java type
	 */
	Class<Y> getJavaType();

	/**
	 * Return the java.lang.reflect.Member for the represented
	 * attribute.
	 *
	 * @return corresponding java.lang.reflect.Member
	 */
	java.lang.reflect.Member getJavaMember();

	/**
	 * Is the attribute an association.
	 *
	 * @return whether boolean indicating whether attribute
	 *         corresponds to an association
	 */
	boolean isAssociation();

	/**
	 * Is the attribute collection-valued.
	 *
	 * @return boolean indicating whether attribute is
	 *         collection-valued
	 */
	boolean isCollection();
}
