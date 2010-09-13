// $Id: SecondaryTable.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation is used to specify a secondary table for the annotated entity class. Specifying
 * one or more secondary tables indicates that the data for the entity class is stored across multiple
 * tables.
 *
 * If no SecondaryTable annotation is specified, it is assumed that all persistent fields or properties
 * of the entity are mapped to the primary table. If no primary key join columns are specified, the
 * join columns are assumed to reference the primary key columns of the primary table, and have the
 * same names and types as the referenced primary key columns of the primary table.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE}) @Retention(RUNTIME)
public @interface SecondaryTable {
	/**
	 * The name of the table
	 */
	String name();
	/**
	 * The catalog of the table
	 */
	String catalog() default "";
	/**
	 * The schema of the table
	 */
	String schema() default "";
	/**
	 * The columns that are used to join with the primary table.
	 *
	 * Defaults to the column(s) of the same name(s) as the primary key column(s)
	 * in the primary table
	 */
	PrimaryKeyJoinColumn[] pkJoinColumns() default {};
	/**
	 * Unique constraints that are to be placed on the table. These are typically only used if
	 * table generation is in effect. These constraints apply in addition to any constraints
	 * specified by the Column and JoinColumn annotations and constraints entailed by primary
	 * key mappings.
	 *
	 * Defaults to no additional constraints.
	 */
	UniqueConstraint[] uniqueConstraints() default {};
}
