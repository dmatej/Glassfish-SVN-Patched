/*
 * @(#)file      MBeanServerInt.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.18
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

import javax.management.MBeanServerDelegate;
import javax.management.*;
import java.io.ObjectInputStream;
import java.util.Set;

import com.sun.jdmk.interceptor.DefaultMBeanServerInterceptor;
import com.sun.jdmk.interceptor.MBeanServerInterceptor;

/**
 * Extends the MBeanInterceptor interface to provide methods for
 * getting and setting the DefaultMBeanInterceptor used for request
 * treatment, getters for the MetaData and MBeanInstantiator objects
 * associated to the MBeanServer and a getter for the class loader of
 * an MBean given its ObjectName.
 *
 * @since Java DMK 5.0
 * @deprecated Use {@link com.sun.jdmk.JdmkMBeanServer} instead.
 */

public interface MBeanServerInt extends MBeanServerInterceptor  {

    /**
     * Return the MBeanInterceptor to which all requests from the
     * MBeanServer interface are forwarded.
     * @deprecated use {@link com.sun.jdmk.JdmkMBeanServer#getMBeanServerInterceptor()}
     */
    public MBeanInterceptor getDefaultMBeanInterceptor();
    

    /**
     * <p>Change the MBeanInterceptor object to which all requests
     * from the MBeanServer interface are forwarded.  The old 
     * object is lost unless it was accessible by other means.</p>
     *
     * @param interceptor   the new object.
     * @exception IllegalArgumentException if <code>interceptor</code> is null.
     * @deprecated use {@link com.sun.jdmk.JdmkMBeanServer#setMBeanServerInterceptor(com.sun.jdmk.interceptor.MBeanServerInterceptor)}
     */
    public  void setDefaultMBeanInterceptor(MBeanInterceptor interceptor) 
	throws IllegalArgumentException;


    /**
     * Return the MBeanInstantiator associated to this MBeanServer.
     */
    public MBeanInstantiator getMBeanInstantiator();


    /**
     * Return the MetaData associated to this MBeanServer.
     */
    public MetaData getMetaData();

    /**
     * Return the MBeanServerDelegate representing the MBeanServer
     */
    public javax.management.MBeanServerDelegate getMBeanServerDelegate();


}
