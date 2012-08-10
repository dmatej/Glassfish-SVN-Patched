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
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.persistence;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.config.SessionCustomizer;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.logging.SessionLog;
import org.eclipse.persistence.logging.SessionLogEntry;
import org.eclipse.persistence.sessions.Session;

/**
 * <b>Purpose</b>: Capture EclipseLink SQL statements for the current thread.
 * <p>
 * <b>Description</b>: This utility will wrap the {@link SessionLog} of any
 * EclipseLInk shared session with a {@link Proxy} allowing it to intercept all
 * log message requests and when activated by calling {@link #start()} will
 * capture all SQL log messages and store them in {@link #statements}
 * <p>
 * <b>Usage</b>: This utility can be enabled within a {@link SessionCustomizer}
 * or at any point during the usage of EclipseLink JPA persistence unit or
 * native session. To access the native session within an {@link EntityManager}
 * or {@link EntityManagerFactory} use the {@link JpaHelper}.
 * 
 * @see JpaHelper#getServerSession(javax.persistence.EntityManagerFactory)
 * @see JpaHelper#getEntityManager(javax.persistence.EntityManager)
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2 - Should work with all versions of EclipseLink 1.0
 *        and above
 */
public class SQLCapture implements InvocationHandler {

    /**
     * Lookup the {@link SQLCapture} used to wrap the {@link SessionLog} of the
     * provided session. If none is found create and install one.
     * 
     * @return the enabled SQLCapture for the provided session.
     */
    public static SQLCapture enable(Session session) {
        SQLCapture handler = null;

        try {
            Object sessionLogHandler = Proxy.getInvocationHandler(session.getSessionLog());
            if (sessionLogHandler instanceof SQLCapture) {
                return (SQLCapture) sessionLogHandler;
            }
        } catch (IllegalArgumentException iae) {
            // Ignore
        }

        handler = new SQLCapture(session.getSessionLog());
        SessionLog proxy = (SessionLog) Proxy.newProxyInstance(session.getPlatform().getConversionManager().getLoader(), new Class[] { SessionLog.class }, handler);
        session.setSessionLog(proxy);

        return handler;
    }

    /**
     * Original SessionLog. All {@link LogEntry} are passed to this after any
     * capturing of SQL messages is performed.
     */
    private SessionLog original;

    /**
     * List of SQL strings stored on a thread specific basis.
     */
    private ThreadLocal<List<String>> statements = new ThreadLocal<List<String>>();

    private SQLCapture(SessionLog original) {
        this.original = original;
    }

    public SessionLog getOriginal() {
        return original;
    }

    /**
     * @return the SQL statements captured for this thread since the most recent
     *         {@link #start()} call. If {@link #start()}has not been called on
     *         this thread or #stop has been returns null.
     */
    public List<String> getStatements() {
        return this.statements.get();
    }

    /**
     * Start capturing SQL for this thread.
     */
    public void start() {
        this.statements.set(new ArrayList<String>());
    }

    /**
     * 
     * @return the SQL statements captured for this thread since the most recent
     *         {@link #start()} call. If {@link #start()}has not been called on
     *         this thread returns null.
     */
    public List<String> stop() {
        List<String> stmts = getStatements();
        this.statements.set(null);
        return stmts;
    }

    /**
     * Remove the SesisonLog wrapper placing the original back.
     */
    public void remove() {
        if (this.original != null) {
            this.original.getSession().setSessionLog(this.original);
        }
    }

    /**
     * INTERNAL
     * <p>
     * {@link InvocationHandler} for the dynamic {@link Proxy} wrapping the
     * original {@link SessionLog}
     * 
     * @see java.lang.reflect.InvocationHandler#invoke(java.lang.Object,
     *      java.lang.reflect.Method, java.lang.Object[])
     */
    public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
        if (getStatements() != null && method.getName().equals("log")) {
            SessionLogEntry entry = (SessionLogEntry) args[0];
            if (SessionLog.SQL.equals(entry.getNameSpace())) {
                getStatements().add(entry.getMessage().trim());
            }
        }

        return method.invoke(this.original, args);
    }

}
