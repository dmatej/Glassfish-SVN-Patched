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
 * The <code>MapKeyClass</code> annotation is used to specify the type of the
 * map key for associations of type java.util.Map. The map key can be a basic
 * type, an embeddable class, or an entity. If the map is specified using
 * Java generics, the <code>MapKeyClass</code> annotation and associated type
 * need not be specified; otherwise they must be specified.
 * The <code>MapKeyClass</code> annotation is not used when {@link MapKey} is 
 * specified and vice versa.
 *
 * <pre>
 * <p>Example 1:
 * 
 *   &#064;Entity
 *   public class Item {
 *     &#064;Id int id;
 *     ...
 *     &#064;ElementCollection(targetClass=String.class)
 *     &#064;MapKeyClass(String.class)
 *     Map images; // map from image name to image filename
 *     ...
 *   }
 * <p> Example 2:
 * 
 * // MapKeyClass and target type of relationship can be defaulted
 * 
 *   &#064;Entity
 *   public class Item {
 *     &#064;Id int id;
 *     ...
 *     &#064;ElementCollection
 *     Map&#60;String, String> images;
 *     ...
 *   }
 *  
 * <p> Example 3:
 * 
 *   &#064;Entity
 *   public class Company {
 *     &#064;Id int id;
 *     ...
 *     &#064;OneToMany(targetEntity=com.example.VicePresident.class)
 *     &#064;MapKeyClass(com.example.Division.class)
 *     Map organization;
 *   }
 * 
 * Example 4:
 * 
 * // MapKeyClass and target type of relationship are defaulted
 * 
 *   &#064;Entity
 *   public class Company {
 *     &#064;Id int id;
 *     ...
 *     &#064;OneToMany
 *     Map&#60;Division, VicePresident> organization;
 * }
 * 
 *  </pre>
 * 
 * 
 * @see OneToMany
 * @see ManyToMany
 * @see ElementCollection
 * @see MapKey
 * 
 * 
 * 
 * @since Java Persistence 2.0
 * 
 * 
 */
@Target({METHOD, FIELD})
@Retention(RUNTIME)

public @interface MapKeyClass {

    /**
     * The type of the map key for associations of type java.util.Map
     */
    Class value() default void.class;
}
