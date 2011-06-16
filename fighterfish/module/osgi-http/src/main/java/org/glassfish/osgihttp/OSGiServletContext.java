/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2009-2010 Oracle and/or its affiliates. All rights reserved.
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

import com.sun.enterprise.web.ContextFacade;
import com.sun.enterprise.web.WebModule;
import org.apache.catalina.LifecycleException;
import org.apache.catalina.session.StandardManager;
import org.osgi.service.http.HttpContext;

import javax.servlet.*;
import javax.servlet.descriptor.JspConfigDescriptor;
import java.io.File;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Unlike Java EE Web Application model, there is no notion of "context path"
 * in OSGi HTTP service spec. Here the servlets can specify which context they
 * belong to by passing a {@link org.osgi.service.http.HttpContext} object.
 * Those HttpContext objects don't have any "path" attribute. As a result,
 * all the OSGi/HTTP servlets belonging to the same servlet context may not
 * have any of the path common to them. Internally, we register all the OSGi
 * servlets (actually we register {@link OSGiServletWrapper}
 * with the same {@link org.apache.catalina.Context} object. So we need a way to
 * demultiplex the OSGi servlet context. This class also delegates to
 * {@link HttpContext} for resource resolutions and security.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class OSGiServletContext extends ContextFacade {

    private final HttpContext httpContext;

    private final Map<String, Object> attributes = new ConcurrentHashMap<String, Object>();

    public OSGiServletContext(WebModule delegate, HttpContext httpContext) {
        super(new File(delegate.getDocBase()), delegate.getContextPath(), delegate.getClassLoader());
        setUnwrappedContext(delegate);
        setName(delegate.getName());
        setPath(delegate.getPath());
        setWebContainer(delegate.getWebContainer());
        setJ2EEServer(delegate.getJ2EEServer());
        setWebModuleConfig(delegate.getWebModuleConfig());
        setParentClassLoader(delegate.getParentClassLoader());
        setRealm(delegate.getRealm());
        setParent(delegate.getParent());
        // Set a new manager to have a different HttpSession for this context
        StandardManager mgr = new StandardManager();
        mgr.setPathname(null); // we switch off Session Persistence due to issues in deserialization
        setManager(mgr);
//        mgr.setMaxActiveSessions(100);
        this.httpContext = httpContext;
    }

    public Object getAttribute(String name) {
        return attributes.get(name);
    }

    public void setAttribute(String name, Object value) {
        attributes.put(name, value);
    }

    public Enumeration getAttributeNames() {
        return Collections.enumeration(attributes.keySet());
    }

    public void removeAttribute(String name) {
        attributes.remove(name);
    }

    public String getMimeType(String file) {
        String mimeType = httpContext.getMimeType(file);
        return mimeType != null ? mimeType : super.getMimeType(file);
    }

    public URL getResource(String path) throws MalformedURLException {
        return httpContext.getResource(path);
    }

    public InputStream getResourceAsStream(String path) {
        try {
            URL url = getResource(path);
            return url != null ? url.openStream() : null;
        } catch (Exception e) {
        }
        return null;
    }

}
