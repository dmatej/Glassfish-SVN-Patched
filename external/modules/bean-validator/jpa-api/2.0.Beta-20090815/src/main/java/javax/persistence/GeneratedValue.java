// $Id: GeneratedValue.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * Provides for the specification of generation strategies for the values of primary keys.
 * The GeneratedValue annotation may be applied to a primary key property or field of an entity
 * or mapped superclass in conjunction with the Id annotation.
 *
 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface GeneratedValue {
	/**
	 * The primary key generation strategy that the persistence provider must use
	 * to generate the annotated entity primary key.
	 */
	GenerationType strategy() default GenerationType.AUTO;
	/**
	 * The name of the primary key generator to use as specified in the SequenceGenerator or
	 * TableGenerator annotation.
	 *
	 * Defaults to the id generator supplied by persistence provider. 
	 */
	String generator() default "";
}
