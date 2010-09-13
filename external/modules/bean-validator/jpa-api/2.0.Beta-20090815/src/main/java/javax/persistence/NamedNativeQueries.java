// $Id: NamedNativeQueries.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * Is used to specify an array of native SQL named queries. Query names are scoped to the persistence unit
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE}) @Retention(RUNTIME)
public @interface NamedNativeQueries {
	/**
	 * Array of native SQL named queries
	 */
	NamedNativeQuery [] value ();
}
