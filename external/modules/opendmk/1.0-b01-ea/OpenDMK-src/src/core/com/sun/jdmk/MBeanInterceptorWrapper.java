/*
 * @(#)file      MBeanInterceptorWrapper.java
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

// java import
import java.util.Iterator;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.lang.reflect.InvocationTargetException; 
import java.lang.reflect.Method; 
import java.lang.reflect.Constructor;
import java.io.OptionalDataException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException ;

// JMX import
import javax.management.*; 

import com.sun.jdmk.interceptor.MBeanServerInterceptor;
import com.sun.jdmk.interceptor.CompatibleMBeanInterceptor;
import com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper;

/**
 * Implements a MBeanInterceptorWrapper that receives all requests from
 * the MBeanServer and forward them to a wrapped MBeanInterceptor.
 * <p>
 * If the MBeanInterceptorWrapper is instantiated with a non null
 * {@link com.sun.jdmk.MBeanInterceptorWrapper.Controller} object,
 * then <i>beginOperation</i> is called on the controller before any
 * operation is invoked on the wrapped interceptor, and <i>endOperation</i>
 * is called on the controller when the operation invoked on the wrapped
 * interceptor is completed.
 * <p>
 * The controller object can be used, for instance, to determine how
 * many operations are currently active on the wrapped interceptor.
 * @see com.sun.jdmk.MBeanInterceptorWrapper.Controller
 *
 * @since Java DMK 5.0
 * @deprecated use {@link com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper} instead.
 */

public class MBeanInterceptorWrapper 
    extends MBeanServerInterceptorWrapper
    implements MBeanInterceptor {

    /**
     * This interface is called before and after each operation is invoked
     * on the wrapped interceptor.
     *
     * @deprecated Use {@link
     * com.sun.jdmk.interceptor.MBeanServerInterceptorWrapper.Controller}
     * instead.
     *
     * @see com.sun.jdmk.MBeanInterceptorWrapper
     *
     * @since Java DMK 5.0
     **/
    public interface Controller 
	extends MBeanServerInterceptorWrapper.Controller {
    }

    private static MBeanServerInterceptor wrap(MBeanInterceptor mbi) {
	if (mbi == null) throw new
	    IllegalArgumentException("MBeanInterceptor is null");
	if (mbi instanceof MBeanServerInterceptor)
	    return  (MBeanServerInterceptor) mbi;
	else return new CompatibleMBeanInterceptor(mbi);
    }

    /**
     * Construct a new MBeanInterceptorWrapper.
     * @param interceptor   The wrapped interceptor.
     * @param controller The (possibly null) controller object on which 
     *        <i>beginOperation</i> and <i>endOperation</i> should be
     *        called. If <code>controller==null</code>, then
     *        <i>beginOperation</i> and <i>endOperation</i> are not called.
     **/
    public MBeanInterceptorWrapper(final MBeanInterceptor  interceptor,
				   final Controller controller) {
	super(wrap(interceptor),controller);
    }
   
}
