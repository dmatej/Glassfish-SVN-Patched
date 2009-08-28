//$Id: AssertFalse.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * The annotated property has to be false.
 *
 * @author Gavin King
 */
@Documented
@ValidatorClass(AssertFalseValidator.class)
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface AssertFalse {
	String message() default "{validator.assertFalse}";
}
