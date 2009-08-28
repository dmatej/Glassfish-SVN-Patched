package javax.validation.constraints;

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.Documented;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import javax.validation.Constraint;

/**
 * The annotated String must match the following regular expression.
 * The regular expression follows the Java regular expression conventions
 * see {@link java.util.regex.Pattern}.
 *
 * Accepts String. <code>null</code> elements are considered valid.
 *
 * @author Emmanuel Bernard
 */
@Target({ METHOD, FIELD, ANNOTATION_TYPE })
@Retention(RUNTIME)
@Documented
@Constraint(validatedBy = {})
public @interface Pattern {
	/**
	 * regular expression to match
	 */
	String regexp();

	/**
	 * Flags considered when resolving the regular expression
	 */
	Flag[] flags() default {};

	/**
	 * error message template
	 */
	String message() default "{constraint.pattern}";

	/**
	 * groups the constraint belongs to
	 */
	Class<?>[] groups() default { };

	/**
	 * Possible Regexp flags
	 */
	public static enum Flag {
		/**
		 * Enables Unix lines mode
		 * @see java.util.regex.Pattern#UNIX_LINES
		 */
		UNIX_LINES,

		/** 
		 * Enables case-insensitive matching
		 * @see java.util.regex.Pattern#CASE_INSENSITIVE
		 */
		CASE_INSENSITIVE,

		/**
		 * Permits whitespace and comments in pattern
		 * @see java.util.regex.Pattern#COMMENTS
		 */
		COMMENTS,

		/**
		 * Enables multiline mode
		 * @see java.util.regex.Pattern#MULTILINE
		 */
		MULTILINE,

		/**
		 * Enables dotall mode
		 * @see java.util.regex.Pattern#DOTALL
		 */
		DOTALL,

		/**
		 * Enables Unicode-aware case folding
		 * @see java.util.regex.Pattern#UNICODE_CASE
		 */
		UNICODE_CASE,

		/**
		 * Enables canonical equivalence
		 * @see java.util.regex.Pattern#CANON_EQ
		 */
		CANON_EQ
	}

	/**
	 * Defines several @Pattern annotations on the same element
	 * @see @link Pattern}
	 *
	 * @author Emmanuel Bernard
	 */
	@Target({ METHOD, FIELD, ANNOTATION_TYPE })
	@Retention(RUNTIME)
	@Documented
	@interface List {
		Pattern[] value();
	}
}
