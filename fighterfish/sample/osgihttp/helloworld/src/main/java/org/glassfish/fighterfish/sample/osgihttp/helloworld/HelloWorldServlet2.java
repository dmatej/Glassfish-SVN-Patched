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

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

/**
 * This servlet is very similar to {@link HelloWorldServlet1} except that it uses
 * {@link HttpServletRequest#getSession(boolean)#getServletContext()} to retrieve the
 * ServletContext and from there it reads the attribute values to demonstrate that it does 
 * not mater how you retrieve the ServletContext, they are all functionally equivalent.
 * 
 * @author sanjeeb.sahoo@oracle.com
 *
 */
public class HelloWorldServlet2 extends HttpServlet {

	final static String AttrName = "count";

	public void init(ServletConfig sc) throws ServletException {
		System.out.println(this + ".init(" + sc + ")");
		super.init(sc);
	}

	public void destroy() {
		System.out.println(this + ".destroy()");
	}

	protected void service(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		resp.setContentType("text/html");
		PrintWriter writer = resp.getWriter();
		writer.write("<html><body><p>Hello World -- sample servlet #2: </p>");
		ServletContext sc = req.getSession(true).getServletContext();
		Integer count = (Integer) sc.getAttribute(AttrName);
		writer.write("<p>servlet context counter = " + count + "</p>");
		if (count == null) {
			count = 0;
		}
		sc.setAttribute(AttrName, new Integer(++count));

		HttpSession session = req.getSession(true);
		Integer sessionCount = (Integer)session.getAttribute(AttrName);
		writer.write("<p>http session counter = " + sessionCount + "</p>");
		if (sessionCount == null) {
			sessionCount = new Integer(0);
		}
		session.setAttribute(AttrName, new Integer(sessionCount.intValue() + 1));
		writer.print("</body></html>");
	}

}
