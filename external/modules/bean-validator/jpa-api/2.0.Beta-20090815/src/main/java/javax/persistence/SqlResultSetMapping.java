// $Id: SqlResultSetMapping.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.ElementType;
import java.lang.annotation.RetentionPolicy;

/**
 * This annotation is used to specify the mapping of the result of a native SQL query
 *
 * @author Emmanuel Bernard
 */
@Target({ElementType.TYPE}) @Retention(RetentionPolicy.RUNTIME)
public @interface SqlResultSetMapping {
	/**
	 * The name given to the result set mapping, and used to refer to it in the methods of the Query API
	 */
	String name();
	/**
	 * Specifies the result set mapping to entities
	 */
	EntityResult[] entities() default {};
	/**
	 * Specifies the result set mapping to scalar values
	 */
	ColumnResult[] columns() default {};
}
