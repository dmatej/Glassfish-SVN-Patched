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
/**
 * Defines the values that are used by the {@link Access} annotation.
 * 
 *  @since Java Persistence 2.0
 * 
 */
public enum AccessType {

    /**
     * Access(FIELD) is used to specify that the persistence provider 
     * runtime is to use instance variables access.
     * <p>When Access(FIELD) is applied to an entity class, mapped superclass,
     * or embeddable class, mapping annotations may be placed on the instance
     * variables of that class, and the persistence provider runtime accesses
     * persistent state via the instance variables defined by the class. All 
     * non-transient instance variables that are not annotated with the 
     * Transient annotation are persistent. When Access(FIELD) is applied to 
     * such a class, it is possible to selectively designate individual
     * attributes within the class for property access. To specify a persistent
     * property for access by the persistence provider runtime, that property 
     * must be designated Access(PROPERTY). The behavior is undefined if mapping
     * annotations are placed on any properties defined by the class for which
     * Access(PROPERTY) is not specified. Persistent state inherited from 
     * superclasses is accessed in accordance with the access types of those
     * superclasses.
     */
    FIELD,
    /**
     * Access(PROPERTY) is used to specify that the  persistence provider 
     * runtime is to use property access.
     * <p>When Access(PROPERTY) is applied to an entity class, mapped 
     * superclass, or embeddable class, mapping annotations may be placed on
     * the properties of that class, and the persistence provider runtime
     * accesses persistent state via the properties defined by that class. All 
     * properties that are not annotated with the Transient annotation are
     * persistent. When Access(PROPERTY) is applied to such a class, it is 
     * possible to selectively designate individual attributes within the class
     * for instance variable access. To specify a persistent instance variable
     * for access by the persistence provider runtime, that instance variable
     * must be designated Access(FIELD). The behavior is undefined if mapping 
     * annotations are placed on any instance variables defined by the class
     * for which Access(FIELD) is not specified. Persistent state inherited
     * from superclasses is accessed in accordance with the access types of
     * those superclasses.
     */
    PROPERTY,}

