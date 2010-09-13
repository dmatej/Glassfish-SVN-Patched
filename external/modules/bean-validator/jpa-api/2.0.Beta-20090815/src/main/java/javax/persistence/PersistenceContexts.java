// $Id: PersistenceContexts.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Declares one or more PersistenceContext annotations. It is used to express a dependency on
 * container-managed entity manager persistence contexts.
 *
 * @author <a href="mailto:bill@jboss.org">Bill Burke</a>
 */
@Target({ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
public @interface PersistenceContexts {
	/**
	 * One or more persistence context
	 */
	PersistenceContext[] value();
}
