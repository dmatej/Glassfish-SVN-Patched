package com.sun.enterprise.config.serverbeans.customvalidators;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import java.lang.annotation.Documented;
import static java.lang.annotation.ElementType.TYPE;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import javax.validation.Constraint;
import javax.validation.Payload;

@Retention(RUNTIME)
@Target({METHOD, FIELD, TYPE})
@Documented
@Constraint(validatedBy = FileRealmPropertyCheckValidator.class)
public @interface FileRealmPropertyCheck {
    String message() default "file and jaas-context have to be specified for FileRealm";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}
