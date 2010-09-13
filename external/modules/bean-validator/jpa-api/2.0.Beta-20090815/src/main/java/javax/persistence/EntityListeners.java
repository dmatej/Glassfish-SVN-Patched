// $Id: EntityListeners.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;


/**
 * Specifies the callback listener classes to be used for an entity or mapped superclass.
 * This annotation may be applied to an entity class or mapped superclass.
 *
 * @author <a href="mailto:bill@jboss.org">Bill Burke</a>
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityListeners {
	/**
	 * The callback listener classes
	 */
   Class[] value();
}
