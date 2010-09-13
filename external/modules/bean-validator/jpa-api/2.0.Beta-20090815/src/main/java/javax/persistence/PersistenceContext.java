// $Id: PersistenceContext.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Expresses a dependency on an EntityManager persistence context.
 *
 * @author <a href="mailto:bill@jboss.org">Bill Burke</a>
 */
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
public @interface PersistenceContext {
	/**
	 * The name by which the entity manager is to be accessed in the environment referencing context,
	 * and is not needed when dependency injection is used.
	 */
	String name() default "";
	/**
	 * The name of the persistence unit. If the unitName element is specified, the persistence unit
	 * for the entity manager that is accessible in JNDI must have the same name.
	 */
	String unitName() default "";
	/**
	 * Used to specify properties for the container or persistence provider. Vendor specific
	 * properties may be included in this set of properties. Properties that are not
	 * recognized by a vendor are ignored.
	 */
	PersistenceProperty[] properties() default {};
	/**
	 * Specifies whether this is a transaction-scoped persistence context or
	 * an extended persistence context.
	 */
	PersistenceContextType type() default PersistenceContextType.TRANSACTION;
}
