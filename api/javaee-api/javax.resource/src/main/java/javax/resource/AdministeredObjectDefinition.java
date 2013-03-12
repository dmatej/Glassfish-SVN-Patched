/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012-2013 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
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
package javax.resource;

import static java.lang.annotation.ElementType.TYPE;
import java.lang.annotation.Retention;
import static java.lang.annotation.RetentionPolicy.RUNTIME;
import java.lang.annotation.Target;

/**
 *  Annotation used to define a Connector administered object to be
 *  registered in JNDI.
 *
 *  Once defined, an administered object may be referenced
 *  by a component using the <code>lookup</code> element of the
 *  <code>Resource</code> annotation.
 *
 *  @see javax.annotation.Resource
 *  @version 1.7
 *  @since 1.7
 */
@Retention(RUNTIME)
@Target({TYPE})
public @interface AdministeredObjectDefinition {

    /**
     *  JNDI name of the administered object being defined.
     */
    String name();
    
    /**
     *  Description of the administered object.
     */
    String description() default "";

    /**
     *  The name of the resource adapter that the administered object must be 
     *  created from. The resource adapter is required to be available at 
     *  deployment time.
     */
    String resourceAdapter();

    /**
     *  Fully qualified name of the administered object’s class
     */
    String className();

    /**
     *  Fully qualified name of the administered object’s interface 
     */
    String interfaceName() default "";

    /**
     *  Properties of the administered object.  These properties may be
     *  vendor-specific properties. Vendor-specific properties may be combined 
     *  with or used to override the administered object properties 
     *  defined using this annotation.
     *  
     *  Administered Object properties that are specified and are not supported 
     *  in a given resource adapter or cannot be mapped to a vendor specific 
     *  configuration property may be ignored.  
     */
    String[] properties() default {};
}
