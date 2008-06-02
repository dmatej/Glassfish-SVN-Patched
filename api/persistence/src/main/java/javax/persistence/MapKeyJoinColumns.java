package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Composite map keys referencing entities are supported by means of the 
 * <code>MapKeyJoinColumns</code> annotation.The <code>MapKeyJoinColumns</code>
 * annotation groups {@link MapKeyJoinColumn} annotations.
 * 
 * @since Java Persistence 2.0
 *   
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface MapKeyJoinColumns {
    /**
     * When the <code>MapKeyJoinColumns</code> annotation is used, both the
     * name and the referencedColumnName elements must be specified in each
     * of the grouped {@link MapKeyJoinColumn} annotations.
     * 
     */
    
    MapKeyJoinColumn[] value();
}
