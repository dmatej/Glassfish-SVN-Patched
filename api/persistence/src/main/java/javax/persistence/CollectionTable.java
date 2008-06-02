/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 1997-2008 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package javax.persistence;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * The <code>CollectionTable</code> annotation is used in the mapping of 
 * collections of basic or embeddable types. The <code>CollectionTable</code>
 * annotation specifies the table that is used for the mapping of the 
 * collection and is specified on the collection-valued field or property.
 * 
 * <p>By default, the columns of the <code>CollectionTable</code> that 
 * correspond to the embeddable class or basic type are derived from the
 * attributes of the embeddable class or from the basic type according to
 * the default values of the {@link Column} annotation.
 * 
 * <p>To override the default properties of the column used for a basic type, 
 * the {@link Column} annotation is used on the collection-valued attribute in 
 * addition to the {@link ElementCollection} annotation.</p>
 * 
 * <p>To override these defaults for an embeddable class, the 
 * {@link AttributeOverride}and/or {@link AttributeOverrides}
 * annotations must be used.</p>
 * 
 * <p>If the <code>CollectionTable</code> annotation is missing, the default
 * values of the <code>CollectionTable </code> annotation elements apply.</p>
 * 
 * <pre>
 * <p>Example:
 * 
 *   &#064;Embeddable public class Address {
 *       protected String street;
 *       protected String city;
 *       protected String state;
 *       ...
 *   }
 *   &#064;Entity public class Person {
 *       &#064;Id protected String ssn;
 *       protected String name;
 *       protected Address home;
 * 
 *       &#064;ElementCollection // use default table (PERSON_NICKNAMES)
 *       &#064;Column(name="name", length=50)
 *       protected Set&#60;String> nickNames = new HashSet();
 *       ...
 *   }
 *   &#064;Entity public class WealthyPerson extends Person {
 *       &#064;ElementCollection
 *       &#064;CollectionTable(name="HOMES")
 *       &#064;AttributeOverrides({
 *          &#064;AttributeOverride(name="street",
 *                                  column=&#064;Column(name="HOME_STREET"),
 *          &#064;AttributeOverride(name="city",
 *                                  column=&#064;Column(name="HOME_CITY"),
 *          &#064;AttributeOverride(name="state",
 *                                  column=&#064;Column(name="HOME_STATE")
 *       })
 *          protected Set&#60;Address> vacationHomes = new HashSet();
 *          ...
 *   }
 * </pre> 
 * 
 * @since Java Persistence 2.0
 *
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface CollectionTable {

    /**
     * The name of the CollectionTable.
     * <p>Defaults to the concatenation of the name of the containing entity and
     * the name of the collection attribute, separated by an underscore.
     * 
     */
    String name()  default "";

    /**
     * The catalog of the Collection table.
     * <p>Defaults to default catalog.
     * 
     */
    String catalog()  default "";

    /**
     * The schema of the table.
     * <p>Defaults to default schema for user.
     */
    String schema()  default "";

     /**
     * The foreign key columns of the collection table which reference the primary
     * table of the entity. 
     * <p>The default only applies if a single join column is used. The same 
     * defaults as for JoinColumn(i.e., the concatenation of the following: the
     * name of the entity; "_"; the name of the referenced primary key column.) 
     * However, if there is more than one join column, a JoinColumn annotation 
     * must be specified for each join column using the JoinColumns annotation.
     * Both the name and the referencedColumnName elements must be specified in
     * each such JoinColumn annotation.
     */
    JoinColumn[] joinColumns() default {};

     /**
     * Unique constraints that are to be placed on the table. These are only
     * used if table generation is in effect.
     * <p>Defaults to no additional constraints. 
     *
     */
    UniqueConstraint[] uniqueConstraints() default {};
}
