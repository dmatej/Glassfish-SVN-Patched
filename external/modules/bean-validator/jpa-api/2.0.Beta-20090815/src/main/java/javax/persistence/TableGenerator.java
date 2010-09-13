// $Id: TableGenerator.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.*;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * This annotation defines a primary key generator that may be referenced by name when a generator
 * element is specified for the GeneratedValue annotation. A table generator may be specified on the
 * entity class or on the primary key field or property. The scope of the generator name is global to
 * the persistence unit (across all generator types).
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD})
@Retention(RUNTIME)
public @interface TableGenerator {
	/**
	 * A unique generator name that can be referenced by one or more classes to be the generator for id values
	 */
	String name();
	/**
	 * Name of table that stores the generated id values. Defaults to a name chosen by persistence provider.
	 */
	String table() default "";
	/**
	 * The catalog of the table
	 * Defaults to the default catalog
	 */
	String catalog() default "";
	/**
	 * The schema of the table
	 * Defaults to the default schema for user
	 */
	String schema() default "";
	/**
	 * Name of the primary key column in the table
	 * Defaults to a provider-chosen name
	 */
	String pkColumnName() default "";
	/**
	 * Name of the column that stores the last value generated
	 * Defaults to a provider-chosen name
	 */
	String valueColumnName() default "";
	/**
	 * The primary key value in the generator table that distinguishes this set of generated values from others that may be stored in the table
	 * Defaults to a provider-chosen value to store in the primary key column of the generator table
	 */
	String pkColumnValue() default "";
	/**
	 * The initial value to be used when allocating id numbers from the generator
	 */
	int initialValue() default 0;
	/**
	 * The amount to increment by when allocating id numbers from the generator
	 */
	int allocationSize() default 50;
	/**
	 * Unique constraints that are to be placed on the table. These are only used if table generation is in effect. These constraints apply in addition to primary key constraints
	 * Defaults to no additional constraints
	 */
	UniqueConstraint[] uniqueConstraints() default {};
}
