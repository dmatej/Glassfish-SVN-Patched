// $Id: JoinTable.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation is used in the mapping of associations. It is specified on the owning
 * side of a many-to-many association, or in a unidirectional one-to-many association.
 *
 * If the JoinTable annotation is missing, the default values of the annotation elements apply.
 * The name of the join table is assumed to be the table names of the associated primary tables
 * concatenated together (owning side first) using an underscore.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD})  @Retention(RUNTIME)
public @interface JoinTable {
	/**
	 * The name of the join table.
	 *
	 * Defaults to the concatenated names of the two associated primary entity tables,
	 * separated by an underscore
	 */
	String name() default "";
	/**
	 * The catalog of the table.
	 *
	 * Defaults to the default catalog.
	 */
	String catalog() default "";
	/**
	 * The schema of the table.
	 *
	 * Defaults to the default schema for user.
	 */
	String schema() default "";
	/**
	 * The foreign key columns of the join table which reference the primary table of the
	 * entity owning the association (i.e. the owning side of the association).
	 *
	 * Uses the same defaults as for JoinColumn.
	 */
	JoinColumn[] joinColumns() default {};
	/**
	 * The foreign key columns of the join table which reference the primary table of the entity
	 * that does not own the association (i.e. the inverse side of the association).
	 *
	 * Uses the same defaults as for JoinColumn
	 */
	JoinColumn[] inverseJoinColumns() default {};
	/**
	 * Unique constraints that are to be placed on the table. These are only used if table
	 * generation is in effect.
	 *
	 * Defaults to no additional constraints
	 */
	UniqueConstraint[] uniqueConstraints() default {};
}
