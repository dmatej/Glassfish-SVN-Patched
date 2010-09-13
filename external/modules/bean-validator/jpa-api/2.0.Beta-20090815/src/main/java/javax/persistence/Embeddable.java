// $Id: Embeddable.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;


/**
 * Defines a class whose instances are stored as an intrinsic part of an owning entity and share
 * the identity of the entity. Each of the persistent properties or fields of the embedded object
 * is mapped to the database table for the entity. Only Basic, Column, Lob, Temporal, and
 * Enumerated mapping annotations may portably be used to map the persistent fields or properties
 * of classes annotated as Embeddable.
 *
 * @author Emmanuel Bernard
 */
@Target({ TYPE })
@Retention(RUNTIME)
public @interface Embeddable {
}
