// $Id: EntityResult.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * References an entity in the SELECT clause of a SQL query. If this annotation is used,
 * the SQL statement should select all of the columns that are mapped to the entity object.
 * This should include foreign key columns to related entities. The results obtained when
 * insufficient data is available are undefined.
 *
 * @author Emmanuel Bernard
 */
@Target({}) @Retention(RetentionPolicy.RUNTIME)
public @interface EntityResult {
	/**
	 * The class of the result
	 */
	Class entityClass();
	/**
	 * Maps the columns specified in the SELECT list of the query to the properties or
	 * fields of the entity class.
	 */
	FieldResult[] fields() default {};
	/**
	 * Specifies the column name (or alias) of the column in the SELECT list that is used to
	 * determine the type of the entity instance.
	 */
	String discriminatorColumn() default "";
}
