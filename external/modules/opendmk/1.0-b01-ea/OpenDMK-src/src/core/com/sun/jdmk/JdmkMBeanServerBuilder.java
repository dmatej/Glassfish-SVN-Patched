/* 
 * @(#)file      JdmkMBeanServerBuilder.java 
 * @(#)author    Sun Microsystems, Inc. 
 * @(#)version   1.12 
 * @(#)date      07/04/04 
 * 
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 */ 

package com.sun.jdmk;

import javax.management.MBeanServer;
import javax.management.MBeanServerDelegate;
import javax.management.MBeanServerBuilder;
import javax.management.MBeanServerFactory;

/**
 * This class represents a builder that creates Java DMK 5.1
 * {@link javax.management.MBeanServer} implementations. 
 * <p>The JMX {@link javax.management.MBeanServerFactory} allows 
 * for applications to provide their custom MBeanServer 
 * implementation. This class used to wrap a Java DMK 5.1 MBeanServer
 * around a Sun JMX 1.2 RI MBeanServer, thus adding the possibility 
 * to plug {@link com.sun.jdmk.interceptor.MBeanServerInterceptor}
 * in the MBeanServer.
 *
 * <p>In order to set the Java DMK 5.1 MBeanServerBuilder as default
 * initial builder, you should specify  <code>
 * -Djavax.management.builder.initial=com.sun.jdmk.JdmkMBeanServerBuilder
 * </code> on the Java command line.
 *
 * @see javax.management.MBeanServerFactory
 * @since Java DMK 5.1
 **/
public class JdmkMBeanServerBuilder extends MBeanServerBuilder {

    private final MBeanServerBuilder inner;

    /** 
     * Creates a new Java DMK MBeanServerBuilder. The Java DMK 
     * MBeanServer implementation wraps the JMX MBeanServer implementation,
     * and provides advanced features such as 
     * {@link com.sun.jdmk.interceptor.MBeanServerInterceptor
     * MBeanServerInterceptors}.
     * The JdmkMBeanServerBuilder created by this constructor wraps
     * a new {@link javax.management.MBeanServerBuilder}, from which
     * the JMX MBeanServer is obtained.
     * If you set the Java DMK MBeanServerBuilder as the default builder
     * by specifying <code>
     * -Djavax.management.builder.initial=com.sun.jdmk.JdmkMBeanServerBuilder
     * </code> on the Java command line, this constructor will be called.
     **/
    public JdmkMBeanServerBuilder() {
	this(new javax.management.MBeanServerBuilder());
    }

    /** 
     * Creates a new Java DMK MBeanServerBuilder. The Java DMK 
     * MBeanServer implementation wraps the JMX local implementation,
     * and provides advanced features such as 
     * {@link com.sun.jdmk.interceptor.MBeanServerInterceptor
     * MBeanServerInterceptors}.
     * @param inner The MBeanServerBuilder that builds the wrapped inner
     *        MBeanServers. This parameter must not be null.
     **/
    JdmkMBeanServerBuilder(MBeanServerBuilder inner) {
	if (inner == null) throw new 
	    IllegalArgumentException("Inner MBeanServerBuilder is null");
	this.inner = inner;
    }

    // Comments imported from MBeanServerBuilder
    //
    public MBeanServerDelegate newMBeanServerDelegate() {
	// Revisit: might be safer to "wrap" the delegate from "inner" ?
	return new MBeanServerDelegateImpl(inner.newMBeanServerDelegate());
    }

    // Comments imported from MBeanServerBuilder
    //
    public MBeanServer newMBeanServer(String defaultDomain, 
				      MBeanServer outer, 
				      MBeanServerDelegate delegate) {

	final JdmkMBeanServerImpl mbs = 
	    new JdmkMBeanServerImpl(defaultDomain,outer,delegate);

	final MBeanServer innerMBeanServer = 
	    inner.newMBeanServer(defaultDomain,(outer==null?mbs:outer),
				 delegate);

	mbs.initialize(innerMBeanServer);
	return mbs;
    }

    /**
     * This method sets the JdmkMBeanServerBuilder as the default builder
     * for the {@link javax.management.MBeanServerFactory}, wrapping
     * the current {@link javax.management.MBeanServerBuilder} into a
     * new JdmkMBeanServerBuilder.
     **/
//     public static void init() {
// 	MBeanServerBuilder inner = 
// 	    MBeanServerFactory.getMBeanServerBuilder();
// 	if (inner instanceof JdmkMBeanServerBuilder)
// 	    return;
// 	else if (inner == null) 
// 	    inner = new javax.management.MBeanServerBuilder();
// 	final MBeanServerBuilder jdmkBuilder =
// 	    new JdmkMBeanServerBuilder(inner);
// 	MBeanServerFactory.setMBeanServerBuilder(jdmkBuilder);
//     }
} 
