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
 * 
 * The <code>MapKeyJoinColumn</code> annotation is used to specify a table of 
 * the target entity mapping to an entity that is a map key. The map key join 
 * column is in the collection table, join table, that is used to represent
 * the map. If no <code>MapKeyJoinColumn</code> annotation is specified, a 
 * single join column is assumed and the default values apply.
 * <pre>
 * <p>Example 1:
 * 
 *   &#064;Entity
 *   public class Company {
 *     &#064;Id int id;
 *     ...
 *     &#064;OneToMany // unidirectional
 *     &#064;JoinTable(name="COMPANY_ORGANIZATION",
 *             joinColumns=&#064;JoinColumn(name="COMPANY"),
 *             inverseJoinColumns=&#064;JoinColumn(name="VICEPRESIDENT"))
 *     &#064;MapKeyJoinColumn(name="DIVISION")
 *     Map&#60;Division, VicePresident> organization;
 *   }
 * 
 * <p>Example 2:
 * 
 *   &#064;Entity
 *   public class VideoStore {
 *     &#064;Id int id;
 *     String name;
 *     Address location;
 *     ...
 *     &#064;ElementCollection
 *     &#064;CollectionTable(name="INVENTORY",
 *             joinColumns=&#064;JoinColumn(name="STORE"))
 *     &#064;Column(name="COPIES_IN_STOCK")
 *     &#064;MapKeyJoinColumn(name="MOVIE", referencedColumnName="ID")
 *     Map&#60;Movie, Integer> videoInventory;
 *     ...
 *   }
 * 
 *   &#064;Entity
 *   public class Movie {
 *     &#064;Id long id;
 *     String title;
 *     ...
 *   }
 *  
 * <p>Example 3:
 * 
 *   &#064;Entity
 *   public class Student {
 *     &#064;Id int studentId;
 *     ...
 *     &#064;ManyToMany // students and courses are also many-many
 *     &#064;JoinTable(name="ENROLLMENTS",
 *                     joinColumns=&#064;JoinColumn(name="STUDENT"),
 *                     inverseJoinColumns=&#064;JoinColumn(name="SEMESTER"))
 *     &#064;MapKeyJoinColumn(name="COURSE")
 *     Map&#60;Course, Semester> enrollment;
 *     ...
 *     }
 * </pre> 
 * @since Java Persistence 2.0
 *  
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface MapKeyJoinColumn {

    /**
     * 
     * (Optional)The name of the foreign key column for the map key. The table
     *  in which it is found depends upon the context. If the join is for a map
     *  key for an element collection, the foreign key column is in the 
     *  collection table for the map value. If the join is for a map key for a
     *  ManyToMany entity relationship or for a OneToMany entity relationship
     *  using a join table, the foreign key column is in a join table. If the
     *  join is for a OneToMany entity relationship using a foreign key mapping
     *  strategy, the foreign key column for the map key is in the table of the
     *  entity that is the value of the map.
     * <p>Defaults to the concatenation of the following: the name of the 
     *  referencing relationship property or field of the referencing entity
     *  or embeddable class;  "_"; "KEY".
     * 
     */
    String name()  default "";

    /**
     * (Optional)The name of the column referenced by this foreign key column. 
     *  The referenced column is in the table of the target entity.
     * <p>Default is the same name as the primary key column of the 
     *  referenced table.
     */
    String referencedColumnName()  default "";

    /**
     * (Optional)Whether the property is a unique key. This is a shortcut for
     *  the UniqueConstraint annotation at the table level and is useful for 
     *  when the unique key constraint is only a single field.
     *  
     */
    boolean unique()  default false;

    /**
     *(Optional)Whether the foreign key column is nullable.
     *   
     */
    boolean nullable()  default false;

    /**
     * (Optional)Whether the column is included in SQL INSERT statements 
     *  generated by the persistence provider.
     *  
     */
    boolean insertable()  default true;

    /**
     * (Optional)Whether the column is included in SQL UPDATE statements
     *  generated by the persistence provider.
     * 
     */
    boolean updatable()  default true;

    /**
     * (Optional)The SQL fragment that is used when generating the DDL for 
     *  the column.
     *  <p>Default is generated SQL for the column.
     * 
     */
    String columnDefinition()  default "";

    /**
     * (Optional)The name of the table that contains the foreign key column. 
     *  <p>By default, if the map is for an element collection, the name of
     *  the collection table for the map value. If the map is for a 
     *  OneToMany or ManyToMany entity relationship using a join table, the 
     *  name of the join table for the map. If the map is for a OneToMany
     *  entity relationship using a foreign key mapping strategy, the name of
     *  the primary table of the entity that is the value of the map.
     * 
     */
    String table()  default "";
}


