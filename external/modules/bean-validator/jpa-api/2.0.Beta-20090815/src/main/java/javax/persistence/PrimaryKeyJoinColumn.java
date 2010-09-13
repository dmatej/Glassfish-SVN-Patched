// $Id: PrimaryKeyJoinColumn.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation specifies a primary key column that is used as a foreign key to join to another
 * table.
 *
 * It is used to join the primary table of an entity subclass in the JOINED mapping strategy to
 * the primary table of its superclass; it is used within a SecondaryTable annotation to join a
 * secondary table to a primary table; and it may be used in a OneToOne mapping in which the
 * primary key of the referencing entity is used as a foreign key to the referenced entity.
 *
 * If no PrimaryKeyJoinColumn annotation is specified for a subclass in the JOINED mapping
 * strategy, the foreign key columns are assumed to have the same names as the primary key
 * columns of the primary table of the superclass
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface PrimaryKeyJoinColumn {
	/**
	 * The name of the primary key column of the current table.
	 *
	 * Defaults to the same name as the primary key column of the primary table of the
	 * superclass (JOINED mapping strategy); the same name as the primary key column of
	 * the primary table (SecondaryTable mapping); or the same name as the primary key
	 * column for the table for the referencing entity (OneToOne mapping)
	 */
	String name() default "";
	/**
	 * The name of the primary key column of the table being joined to.
	 *
	 * Defaults to the same name as the primary key column of the primary table of the
	 * superclass (JOINED mapping strategy); the same name as the primary key column of the
	 * primary table (SecondaryTable mapping); or the same name as the primary key column for
	 * the table for the referencing entity (OneToOne mapping)
	 */
	String referencedColumnName() default "";
	/**
	 * The SQL fragment that is used when generating the DDL for the column. This should not be
	 * specified for a OneToOne primary key association.
	 *
	 * Defaults to the generated SQL to create a column of the inferred type.
	 */
	String columnDefinition() default "";
}
