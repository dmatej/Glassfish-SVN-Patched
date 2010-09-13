//$Id: AssertTrue.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 * The annotated element has to be true
 *
 * @author Gavin King
 */
@Documented
@ValidatorClass(AssertTrueValidator.class)
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface AssertTrue {
	String message() default "{validator.assertTrue}";
}
