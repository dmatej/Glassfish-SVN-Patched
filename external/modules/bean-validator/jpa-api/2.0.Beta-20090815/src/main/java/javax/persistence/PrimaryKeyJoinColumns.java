// $Id: PrimaryKeyJoinColumns.java 16130 2009-03-10 14:28:07Z hardy.ferentschik $
// EJB3 Specification Copyright 2004-2009 Sun Microsystems, Inc.
package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * This annotation groups PrimaryKeyJoinColumn annotations. It is used to map composite foreign keys.
 *
 * @author Emmanuel Bernard
 */
@Target({TYPE, METHOD, FIELD}) @Retention(RUNTIME)
public @interface PrimaryKeyJoinColumns {
	/**
	 * One or more PrimaryKeyJoinColumn annotations
	 */
	PrimaryKeyJoinColumn[] value();
}
