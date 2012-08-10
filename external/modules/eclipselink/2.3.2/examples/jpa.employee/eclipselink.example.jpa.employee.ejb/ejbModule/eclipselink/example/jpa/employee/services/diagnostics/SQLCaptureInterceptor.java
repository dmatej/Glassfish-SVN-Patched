/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - TODO
 ******************************************************************************/
package eclipselink.example.jpa.employee.services.diagnostics;

import java.util.ArrayList;
import java.util.List;

import javax.interceptor.AroundInvoke;
import javax.interceptor.InvocationContext;

import org.eclipse.persistence.jpa.JpaEntityManager;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.sessions.server.Server;

import eclipselink.example.jpa.employee.persistence.SQLCapture;
import eclipselink.example.jpa.employee.services.EmployeeBaseService;

/**
 * Diagnostic intercepter used to capture the SQL calls executed by EclipseLink
 * within a Session Bean method. This is not generally part of EJB application
 * that uses EclipseLink but provided within this example application so that
 * the user interface can display the SQL caused by any service operation.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class SQLCaptureInterceptor {

    /**
     * determine if SQL should be captured.
     */
    private Server getServer(InvocationContext context) {
        if (context.getTarget() instanceof EmployeeBaseService) {
            EmployeeBaseService service = (EmployeeBaseService) context.getTarget();
            JpaEntityManager em = JpaHelper.getEntityManager(service.getEntityManager());
            if (em != null) {
                return em.getServerSession();
            }
        }
        return null;
    }

    @AroundInvoke
    public Object intercept(InvocationContext context) throws Exception {
        Server session = getServer(context);
        SQLCapture log = null;

        // TODO check if enabled
        if (session != null) {
            log = SQLCapture.enable(session);
            log.start();
        }

        return context.proceed();
    }
    
    /**
     * 
     * @param session
     * @return
     */
    public static List<String> getLatestSQL(Server session) {
        if (session == null) {
            return new ArrayList<String>();
        }
        
        SQLCapture log = SQLCapture.enable(session);
        if (log == null || log.getStatements() == null) {
            return new ArrayList<String>();
        }
        return log.getStatements();
    }

}
