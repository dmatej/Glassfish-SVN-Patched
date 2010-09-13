// $Id: Basic.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;
import static javax.persistence.FetchType.EAGER;

/**
 * The Basic annotation is the simplest type of mapping to a database column. The Basic
 * annotation can be applied to a persistent property or instance variable of any of the
 * following types: Java primitive types, wrappers of the primitive types, String,
 * java.math.BigInteger, java.math.BigDecimal, java.util.Date, java.util.Calendar,
 * java.sql.Date, java.sql.Time, java.sql.Timestamp, byte[], Byte[], char[], Character[],
 * enums, and any other type that implements Serializable.
 *
 * The use of the Basic annotation is optional for persistent fields and properties of these types.

 * @author Emmanuel Bernard
 */
@Target({METHOD, FIELD}) @Retention(RUNTIME)
public @interface Basic {
	/**
	 * Defines whether the value of the field or property should be lazily loaded or must be
	 * eagerly fetched. The EAGER strategy is a requirement on the persistence provider runtime
	 * that the value must be eagerly fetched. The LAZY strategy is a hint to the persistence
	 * provider runtime. If not specified, defaults to EAGER.
	 */
	FetchType fetch() default EAGER;
	/**
	 * Defines whether the value of the field or property may be null. This is a hint and is
	 * disregarded for primitive types; it may be used in schema generation. If not specified,
	 * defaults to true.
	 */
	boolean optional() default true;
}
