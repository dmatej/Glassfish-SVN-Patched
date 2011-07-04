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
package org.glassfish.fighterfish.sample.osgihttp.helloworld;

import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.osgi.service.component.ComponentContext;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventAdmin;
import org.osgi.service.http.HttpContext;
import org.osgi.service.http.HttpService;
import org.osgi.service.http.NamespaceException;

/**
 * This class is an SCR component. It registers servlets and resources in
 * activate() method and unregisters them in deactivate() method. It consumes a
 * service of type HttpService. The service reference is bound and unbound in
 * setHttp() and unsetHttp() method respectively as specified in scr.xml file.
 * 
 * @author sanjeeb.sahoo@oracle.com
 * 
 */
public class HelloWorldHttpMain {
	
	private HttpService http; // Set and unset by setHttp() and unsetHttp()
	private EventAdmin eventAdmin;
								// methods that are called by SCR. See scr.xml

	protected void activate(ComponentContext ctx) throws ServletException,
			NamespaceException {
		// One HttpContext maps to one ServletContext.
		// To demonstrate this functionality, we shall do the following:
		// We will create one HttpContext and register Servlet1 and Servlet2 using this.
		// We will then set an attribute in ServletContext and make sure we can read
		// its value inside both Servlet's service().
		// We will create a second HttpContext and register Servlet3 using the second one.
		// We will show that Servlet3's ServletContext does not have the attribute set in Servlet1 or Servlet2's
		// ServletContext. We will also show that Servlet1 and Servlet2 share the same HttpSession which is 
		// different from Servlet3.
		HttpContext httpCtx = http.createDefaultHttpContext();
		HttpServlet servlet1 = new HelloWorldServlet1();
		http.registerServlet("/hello1", servlet1, null, httpCtx);
		System.out.println(servlet1.getServletContext());
		HttpServlet servlet2 = new HelloWorldServlet2();
		http.registerServlet("/hello2", servlet2, null, httpCtx);
		System.out.println(servlet2.getServletContext());
		servlet1.getServletContext().setAttribute(HelloWorldServlet1.AttrName,
				new Integer(0));

		// Let's create another HttpContext and make sure that each context has its own
		// ServletContext and HttpSession.
		HttpContext httpCtx2 = http.createDefaultHttpContext();
		HttpServlet servlet3 = new HelloWorldServlet3();
		http.registerServlet("/hello3", servlet3, null, httpCtx2);
		System.out.println(servlet3.getServletContext());
		assert(servlet3.getServletContext() != servlet1.getServletContext());
		if (eventAdmin != null) { // raise an event so that our test framework can catch it to proceed to test
			Map props = new HashMap();
			Event event = new Event(getClass().getPackage().getName().replace(".", "/"), props);
			eventAdmin.postEvent(event);
			System.out.println("raised event " + event);
		}
	}

	protected void deactivate(ComponentContext ctx) {
		try {
			http.unregister("/hello1");
			http.unregister("/hello2");
			http.unregister("/hello3");
		} catch (Exception e) {
			// This can happen if the HttpService has been undpeloyed in which
			// case as part of its undepoyment,
			// it would have unregistered all aliases. So, we should protect
			// against such a case.
			System.out.println(e);
		}
	}

	protected void setHttp(HttpService http) {
		this.http = http;
	}

	protected void unsetHttp(HttpService http) {
		this.http = null;
	}
	
	protected void setEventAdmin(EventAdmin eventAdmin) {
		this.eventAdmin = eventAdmin;
	}
	
	protected void unsetEventAdmin(EventAdmin eventAdmin) {
		this.eventAdmin = null;
		
	}
}
