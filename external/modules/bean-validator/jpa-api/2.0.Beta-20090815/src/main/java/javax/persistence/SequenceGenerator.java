// $Id: SequenceGenerator.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation defines a primary key generator that may be referenced by name when a generator
 * element is specified for the GeneratedValue annotation. A sequence generator may be specified on
 * the entity class or on the primary key field or property. The scope of the generator name is global
 * to the persistence unit (across all generator types).
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface SequenceGenerator {
	/**
	 * A unique generator name that can be referenced by one or more classes to be the generator for primary key values
	 */
	String name();
	/**
	 * The name of the database sequence object from which to obtain primary key values
	 * Defaults to a provider-chosen value
	 */
	String sequenceName() default "";
	/**
	 * The value from which the sequence object is to start generating
	 */
	int initialValue() default 1;
	/**
	 * The amount to increment by when allocating sequence numbers from the sequence
	 */
	int allocationSize() default 50;
}
