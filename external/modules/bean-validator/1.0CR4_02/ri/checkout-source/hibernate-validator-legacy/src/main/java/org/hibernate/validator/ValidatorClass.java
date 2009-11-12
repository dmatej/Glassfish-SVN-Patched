//$Id: ValidatorClass.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator;

import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;


/**
 * Link between an constraint annotation and it's validator implementation
 *
 * @author Gavin King
 */
@Documented
@Target({ANNOTATION_TYPE})
@Retention(RUNTIME)
public @interface ValidatorClass {
	Class<? extends Validator> value();
}
