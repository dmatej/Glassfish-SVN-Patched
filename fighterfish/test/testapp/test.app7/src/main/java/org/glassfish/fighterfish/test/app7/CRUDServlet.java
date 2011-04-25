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


package org.glassfish.fighterfish.test.app7;

import javax.annotation.Resource;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.UserTransaction;

import java.io.PrintWriter;

/**
 * Servlet implementation class CRUDServlet
 */
@WebServlet("/crud")
public class CRUDServlet extends HttpServlet
{
    /**
     * 
     */
    private static final long serialVersionUID = 9133422911516243972L;

    EntityManagerFactory emf;

    @Resource UserTransaction utx;

    public void service(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, java.io.IOException
    {
        resp.setContentType("text/html");
        PrintWriter out = resp.getWriter();
        out.print("<HTML> <HEAD> <TITLE> Employee-Department CRUD" +
                "</TITLE> </HEAD> <BODY BGCOLOR=white>");
        out.print("\n");

        try {
           EntityManagerFactory emf = Persistence.createEntityManagerFactory("em1");
           utx.begin();
           EntityManager em = emf.createEntityManager();
           out.print(em);
           out.print("\n");
           String action = (String)req.getParameter("action");
           out.print("action = " + action);
           out.print("\n");
           if ("createEmployee".equals(action)) {
              String departmentName = (String)req.getParameter("departmentName");
              Department d = em.find(Department.class, departmentName);
              Employee e = new Employee();
              e.setDepartment(d);
              em.persist(e);
              out.print("Created " + e);
              out.print("\n");
           } else if ("readEmployee".equals(action)) {
              String employeeId = (String)req.getParameter("employeeId");
              Employee e = em.find(Employee.class, Integer.parseInt(employeeId));
              out.print("Found " + e);
              out.print("\n");
           } else if ("deleteEmployee".equals(action)) {
              String employeeId = (String)req.getParameter("employeeId");
              Employee e = em.find(Employee.class, Integer.parseInt(employeeId));
              if (e != null) em.remove(e);
              out.print("Deleted " + e);
              out.print("\n");
           } else if ("createDepartment".equals(action)) {
              String name = (String)req.getParameter("departmentName");
              Department d = new Department(name);
              em.persist(d);
              out.print("Created " + d);
              out.print("\n");
           } else if ("readDepartment".equals(action)) {
              String name = (String)req.getParameter("departmentName");
              Department d = em.find(Department.class, name);
              out.print("Found " + d);
              out.print("\n");
           } else if ("deleteDepartment".equals(action)) {
              String name = (String)req.getParameter("departmentName");
              Department d = em.find(Department.class, name);
              if (d != null) em.remove(d);
              out.print("Deleted " + d);
              out.print("\n");
           }
           utx.commit();
           emf.close();
        }
        catch (Exception e)
        {
            e.printStackTrace(out);
        }
        out.println("</BODY> </HTML> ");

    }
}
