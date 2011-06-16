/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2011 Oracle and/or its affiliates. All rights reserved.
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


package org.glassfish.osgihttp;

import com.sun.enterprise.web.WebModule;
import org.apache.catalina.Request;
import org.apache.catalina.Response;
import org.apache.catalina.Wrapper;
import org.apache.catalina.valves.ValveBase;
import org.osgi.service.http.HttpContext;

import javax.servlet.ServletException;
import java.io.IOException;
import java.lang.reflect.Field;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Since OSGi/HTTP service spec does not a notion of a unique path per http context,
 * we register all the OSGi servlets with the same {@link org.apache.catalina.Context} object.
 * We still need to demultiplex the OSGi servlet context which is separate for each {@link HttpContext}.
 * This valve helps in demultiplexing. It performs following tasks:
 * a) Sets the current WebModule in {@link InvocationContext}
 * b) Sets the current WebModule as the Context of the Request object.
 * c) Reset unsuccessfulSessionFind flag in Request object.
 * <p/>
 * See GLASSFISH-16764 for more details.
 *
 * @author Sanjeeb.Sahoo@Oracle.COM
 */
public class OSGiHttpContextValve extends ValveBase {
    private Logger logger = Logger.getLogger(getClass().getPackage().getName());

    public OSGiHttpContextValve() {
    }

    @Override
    public int invoke(Request request, Response response) throws IOException, ServletException {
        logger.entering("OSGiHttpContextValve", "invoke", new Object[]{request});
        Wrapper wrapper = request.getWrapper();
        if (wrapper instanceof OSGiServletWrapper) {
            final OSGiServletWrapper osgiWrapper = (OSGiServletWrapper) wrapper;
            final WebModule osgiWebModule = osgiWrapper.getWebModule();
            InvocationContextMgr.getInvocationContext().setWebModule(osgiWebModule);
            request.setContext(osgiWebModule);
            resetSessionFindAttr(request);
        }
        return INVOKE_NEXT;
    }

    /**
     * Since we switch the Session Manager midway in the request processing cycle, we have to
     * reset a flag called unsuccessfulSessionFind that's maintained inside the Request object.
     * If we don't reset it, no session corresponding to this OSGi Http Context will be found.
     *
     * @param request
     */
    private void resetSessionFindAttr(Request request) {
        if (request instanceof org.apache.catalina.connector.Request) {
            org.apache.catalina.connector.Request.class.cast(request).setUnsuccessfulSessionFind(false);
        } else {
            logger.logp(Level.FINE, "OSGiHttpContextValve", "resetSessionFindAttr", "request {0} is not of type {1} ",
                    new Object[]{request, org.apache.catalina.connector.Request.class});
        }
    }
}
