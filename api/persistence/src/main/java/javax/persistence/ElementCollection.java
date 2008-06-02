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
import static javax.persistence.FetchType.LAZY;

/**
 * The <code>ElementCollection</code> annotation defines a collection of instances 
 * of a basic type or embeddable class. The <code>ElementCollection</code> is
 * used when the collection is to be mapped by means of a collection table.
 *
 * <pre>
 * <p>Example:
 *   &#064;Entity public class Person {
 *       &#064;Id protected String ssn;
 *       protected String name;
 * 
 *       &#064;ElementCollection
 *       protected Set&#60;String> nickNames = new HashSet();
 *       ...
 * }
 * </pre>
 * 
 * @since Java Persistence 2.0
 * 
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)
public @interface ElementCollection {

    /**
     * (Optional)The basic or embeddable class that is the element type of the
     *  collection. Optional only if the collection field or property is defined
     *  using Java generics. Must be specified otherwise.
     *  <p>Default is the parameterized type of the collection when defined 
     *  using generics.
     * 
     */
    Class targetClass()  

       

default void.class;
    /**
     * (Optional)Whether the collection should be lazily loaded or must be
     *  eagerly fetched. The EAGER strategy is a requirement on the 
     *  persistence provider runtime that the collection elements must be
     *  eagerly fetched. The LAZY strategy is a hint to the persistence 
     *  provider runtime. 
     *  
     */
    FetchType fetch() default LAZY;
}
