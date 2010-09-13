// $Id: DiscriminatorColumn.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;
import static javax.persistence.DiscriminatorType.STRING;

/**
 * Is used to define the discriminator column for the SINGLE_TABLE and JOINED inheritance
 * mapping strategies.
 *
 * The strategy and the discriminator column are only specified in the root of an entity
 * class hierarchy or subhierarchy in which a different inheritance strategy is applied
 *
 * If the DiscriminatorColumn annotation is missing, and a discriminator column is required,
 * the name of the discriminator column defaults to "DTYPE" and the discriminator type to
 * DiscriminatorType.STRING.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE}) @Retention(RUNTIME)
public @interface DiscriminatorColumn {
	/**
	 * The name of column to be used for the discriminator.
	 */
	String name() default "DTYPE";
	/**
	 * The type of object/column to use as a class discriminator.
	 * Defaults to DiscriminatorType.STRING
	 */
	DiscriminatorType discriminatorType() default STRING;
	/**
	 * The SQL fragment that is used when generating the DDL for the discriminator column.
	 * Defaults to the provider-generated SQL to create a column of the specified
	 * discriminator type.
	 */
	String columnDefinition() default "";
	/**
	 * The column length for String-based discriminator types. Ignored for other
	 * discriminator types.
	 */
	int length() default 31;
}
