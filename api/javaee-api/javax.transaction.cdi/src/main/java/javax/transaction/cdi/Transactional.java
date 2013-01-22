/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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
package javax.transaction.cdi;

import javax.enterprise.util.Nonbinding;
import javax.interceptor.InterceptorBinding;
import java.lang.annotation.*;

/**
 * Annotation used by applications to control transaction boundaries on CDI managed beans, as well as
 * classes defined as managed beans by the Java EE specification such as servlets, JAX-RS resource classes, and JAX-WS
 * service endpoints, declaratively.  This provides the semantics of EJB transaction attributes in CDI without
 * dependencies such as RMI.  This support is implemented via an implementation of a CDI interceptor that conducts the
 * necessary suspending, resuming, etc.  The TxType element of the annotation provides the semantic equivalent of the
 * transaction attributes in EJB.
 *
 * By default checked exceptions do not result in the transactional interceptor marking the transaction for rollback
 * and instances of RuntimeException and its subclasses do.  This default behavior can be overridden by specifying
 * which exceptions result in the interceptor marking the transaction for rollback.  The rollbackOn element can be set
 * to indicate which exceptions should cause the interceptor to mark the transaction for rollback.  Conversely, the
 * dontRollbackOn element can be set to indicate which exceptions should not cause the interceptor to mark the
 * transaction for rollback.  When a class is specified for either of these elements, the designated behavior applies
 * to subclasses of that class as well.  If both elements are specified, dontRollbackOn takes precedence.
 *
 * EJB application exceptions (i.e., runtime exceptions annotated with @ApplicationException) are treated just as any
 * other runtime exceptions unless otherwise specified.
 *
 * When Transactional annotated managed beans are used in conjunction with EJB container managed transactions the EJB
 * container behavior is applied before the bean is called.  When the bean is called the CDI behavior is applied before
 * calling the bean's methods.  It is best practice to avoid such use of Transactional annotations in conjunction with
 * EJB container managed transactions in order to avoid possible confusion.
 *
 *
 * @since JTA1.2
 */
@Inherited
@InterceptorBinding
@Target({ElementType.TYPE, ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Transactional {
    TxType value() default TxType.REQUIRED;

    public enum TxType {REQUIRED, REQUIRES_NEW, MANDATORY, SUPPORTS, NOT_SUPPORTED, NEVER}

    @Nonbinding
    public Class[] rollbackOn() default {};

    @Nonbinding
    public Class[] dontRollbackOn() default {};

}
