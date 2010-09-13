// $Id: NamedQuery.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.RetentionPolicy.*;
import static java.lang.annotation.ElementType.*;

/**
 * Is used to specify a named query in the Java Persistence query language, which is a static
 * query expressed in metadata. Query names are scoped to the persistence unit.
 *
 * @author Emmanuel Bernard
 */
//TODO remove the mackage target
@Target({TYPE}) @Retention(RUNTIME)
public @interface NamedQuery {
	/**
	 * Refers to the query when using the EntityManager methods that create query objects.
	 */
	String name();
	/**
	 * The query string in the Java Persistence query language
	 */
	String query();
	/**
	 * Vendor-specific query hints
	 */
	QueryHint[] hints() default {};
}
