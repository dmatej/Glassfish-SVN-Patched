/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
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
package javax.transaction;

/**
 *
 * The TransactionalException thrown from the Transactional interceptors
 *  implementation contains the original exception as its nested exception
 *  and is a RuntimeException, therefore, by default any
 *  transaction that was started as a result of a Transactional annotation
 *  earlier in the call stream will be marked for rollback as a result of
 *  the TransactionalException being thrown by the Transactional interceptor
 *  of the second bean. For example if a transaction is begun as a result of
 *  a call to a bean annotated with Transactional(TxType.REQUIRED) and this
 *  bean in turn calls a second bean annotated with
 *  Transactional(TxType.NEVER), the transaction begun by the first bean
 *  will be marked for rollback.
 *
 * @since JTA1.2
 */
public class TransactionalException extends RuntimeException {
    public TransactionalException(String s, Throwable throwable) {
        super(s, throwable);
    }
}
