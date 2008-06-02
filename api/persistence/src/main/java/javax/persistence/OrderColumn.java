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

import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * The <code>OrderColumn</code> annotation specifies a column that is used 
 * to maintain the persistent order of a list. The persistence provider is
 * responsible for maintaining the order upon retrieval and in the database.
 * The persistence provider is responsible for updating the ordering upon 
 * flushing to the database to reflect any insertion, deletion, or reordering 
 * affecting the list. The <code>OrderColumn</code> annotation is specified 
 * on a one-to-many or many-to-many relationship or on an element collection.
 * The <code>OrderColumn</code> annotation is specified on the side of the
 * relationship that references the collection that is to be ordered. The
 * <code>order column</code> is not visible as part of the state of the entity
 * or embeddable class.
 * <p>The {@link OrderBy} annotation is not used when <code>OrderColumn</code>
 * is specified.
 * <p>If name is not specified, the column name is the concatenation of the
 * following: the name of the referencing relationship property or field of the
 * referencing entity or embeddable class; "_"; "ORDER".
 * <p>The <code>contiguous</code> element specifies whether the values of the 
 * order column need to be <code>contiguous</code> (the default) or may be
 * sparse. If <code>contiguous</code> is true, the order column must be of 
 * integral type. If <code>contiguous</code> is false, the list order cannot be
 * (portably) queried.
 * <p><I>Open Issue: Should <code>contiguous</code> be included in the 
 * spec or not?</I></p>
 * <p>The base element specifies the column value for the first element of the
 * list.
 * <p>The table element specifies the table containing the order column.
 * By default, if the relationship is a many-to-many or unidirectional
 * one-to-many relationship, the table is the join table for the relationship;
 * if the relationship is a bidirectional one-to-many or unidirectional
 * one-to-many mapped by a join column, the table is the primary table for the
 * entity on the many side of the relationship; if the ordering is for a
 * collection of elements, the table is the collection table for the element
 * collection.
 * <pre>
 * <p>Example 1:
 * 
 *   &#064;Entity
 *   public class CreditCard {
 *     &#064;Id long ccNumber;
 *     &#064;OneToMany // unidirectional
 *     &#064;OrderColumn
 *     List&#60;CardTransaction> transactionHistory;
 *     ...
 *   }
 * 
 *<p> Example 2:
 * 
 *   &#064;Entity public class Course {
 *         ...
 *         &#064;ManyToMany
 *         &#064;JoinTable(name="COURSE_ENROLLMENT")
 *         public Set&#60;Student> getStudents() {...};
 *         ...
 *         &#064;ManyToMany // unidirectional
 *         &#064;JoinTable(name="WAIT_LIST")
 *         &#064;OrderColumn("WAITLIST_ORDER")
 *         public List&#60;Student> getWaitList() {...}
 *   }
 * 
 *   &#064;Entity public class Student {
 *         ...
 *         &#064;ManyToMany(mappedBy="students")
 *         public Set&#60;Course> getCourses() {...};
 *         ...
 *   }
 * 
 *   Example of querying the ordered list:
 *   
 *   <I>Note:Query may change with the updated specification</i>
 * 
 *   SELECT s 
 *   FROM course c JOIN c.waitlist w, student s
 *   WHERE c.name = "geometry" AND w[0] = s
 * </pre>
 * 
 * @since Java Persistence 2.0
 * 
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface OrderColumn {
    /**
     * (Optional) The name of the ordering column.
     * Defaults to The concatenation of the name of the referencing property or
     * field; "_"; "ORDER".
     */
    String name() default "";

    /**
     * (Optional) Whether the database column is nullable.
     */
    boolean nullable() default false;

    /**
     * (Optional) Whether the column is included in SQL INSERT statements
     * generated by the persistence provider.
     */
    boolean insertable() default true;

    /**
     * (Optional) Whether the column is included in SQL UPDATE statements
     * generated by the persistence provider.
     */
    boolean updatable() default true;

    /**
     * (Optional) The SQL fragment that is used when generating the DDL for the
     * column.
     * <p>Defaults to generated SQL to create a column of the inferred type.
     */
    String columnDefinition() default "";


    /**
     * (Optional) Whether the value of the ordering column need to be contiguous
     * or may be sparse.
     * 
     */
    boolean contiguous() default true;

    /**
     * (Optional) The ordering column value for the first element of the list.
     */
    int base() default 0;

    /**
     * (Optional) The name of the table that contains the column.
     * Defaults to the table is the join table for the relationship, if the
     * relationship is a many-to-many or unidirectional one-to-many relationship.
     * If the relationship is a bidirectional one-to-many or unidirectional
     * one-to-many mapped by a join column, the table is the primary table for
     * the entity on the many side of the relationship. If the ordering is for
     * a collection of elements, the table is the collection table for the
     * element collection.
     * 
     */
    String table() default "";
}