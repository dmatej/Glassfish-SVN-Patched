// $Id: ColumnResult.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * References name of a column in the SELECT clause of a SQL query - i.e.,
 * column alias, if applicable. Scalar result types can be included in the query
 * result by specifying this annotation in the metadata.
 *
 * @author Emmanuel Bernard
 */
@Target({}) @Retention(RetentionPolicy.RUNTIME)
public @interface ColumnResult {
	/**
	 * The name of a column in the SELECT clause of a SQL query
	 */
	String name();
}
