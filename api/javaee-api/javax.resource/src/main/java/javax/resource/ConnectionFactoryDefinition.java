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

import static javax.resource.spi.TransactionSupport.TransactionSupportLevel;;

/**
 *  Annotation used to define a Connector Connection Factory resource to be 
 *  registered in JNDI.
 *  
 *  Once defined, a resource may be referenced by a component using the
 *  <code>lookup</code> element of the <code>Resource</code> annotation.
 *
 *  @see javax.annotation.Resource
 *  @version 1.7
 *  @since 1.7
 */
@Retention(RUNTIME)
@Target({TYPE})
public @interface ConnectionFactoryDefinition {

    /**
     *  JNDI name of the connection factory resource being defined.
     */
    String name();

    /**
     *  Description of the resource.
     */
    String description() default "";

    /**
     *  The name of the resource adapter that the administered object must be 
     *  created from. The resource adapter is required to be available at 
     *  deployment time.
     */
    String resourceAdapter();

    /**
     *  The fully qualified name of the connection factory interface 
     *  class.
     */
    String interfaceName();
    
    /**
     *  The level of transaction support the connection factory resource 
     *  needs to support. If a transaction support specification is specified, 
     *  it must be a level of transaction support whose ordinal value in the 
     *  <code>TransactionSupport.TransactionSupportLevel</code> enum is equal
     *  to or lesser than the resource adapter's transaction support 
     *  classification.
     */
    TransactionSupportLevel transactionSupport() 
                default TransactionSupportLevel.NoTransaction;
    
    /**
     *  The maximum number of connections that should be allocated for a 
     *  connection pool that backs this connnection factory resource. The 
     *  default for this attribute is vendor specific. 
     */
    int maxPoolSize() default -1;
    
    /**
     *  The minimum number of connections that should be allocated for a 
     *  connection pool that backs this connnection factory resource. The 
     *  default for this attribute is vendor specific. 
     */
    int minPoolSize() default -1;
    
    /**
     *  Properties of the Connection Factory.  These properties may be
     *  vendor-specific properties. Vendor-specific properties may be combined 
     *  with or used to override the connection factory properties 
     *  defined using this annotation.
     *  
     *  Connection Factory properties that are specified and are not supported 
     *  in a given resource adapter or cannot be mapped to a vendor specific 
     *  configuration property may be ignored.  
     */
    String[] properties() default {};
}
