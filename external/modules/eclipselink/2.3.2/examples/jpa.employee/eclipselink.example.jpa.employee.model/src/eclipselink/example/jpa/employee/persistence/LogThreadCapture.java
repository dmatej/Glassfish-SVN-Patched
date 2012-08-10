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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.persistence.logging.SessionLog;
import org.eclipse.persistence.logging.SessionLogEntry;
import org.eclipse.persistence.sessions.Session;

/**
 * <b>Purpose</b>: Capture EclipseLink log statements for the current thread.
 * <p>
 * <b>Description</b>: This utility will wrap the {@link SessionLog} of any
 * EclipseLInk shared session with a {@link Proxy} allowing it to intercept all
 * log message requests and when activated by calling {@link #start()} will
 * capture all SQL log messages and store them in {@link #statements}
 * 
 * @author dclarke
 * @since EclipseLink 2.3
 */
public class LogThreadCapture implements InvocationHandler {

    /**
     * Original SessionLog. All {@link LogEntry} are passed to this after any
     * capturing of SQL messages is performed.
     */
    private SessionLog original;

    /**
     * List of log strings stored on a thread specific basis.
     */
    private ThreadLocal<List<String>> statements = new ThreadLocal<List<String>>();

    /**
     * The level and categories to be captured. By default [*,
     * {@link SessionLog#SQL}] will be captured.
     */
    private Map<Integer, Set<String>> levelCategories;

    private LogThreadCapture(SessionLog original) {
        this.original = original;
        this.levelCategories = new HashMap<Integer, Set<String>>();

    }

    public SessionLog getOriginal() {
        return this.original;
    }

    private Map<Integer, Set<String>> getLevelCategories() {
        return this.levelCategories;
    }

    public Set<String> getCategories(int level) {
        return getLevelCategories().get(level);
    }

    /**
     * 
     * @param level
     * @param categories
     */
    public void capture(int level, String... categories) {
        Set<String> capCats = getCategories(level);

        if (capCats == null) {
            capCats = new HashSet<String>();
            getLevelCategories().put(level, capCats);
        }
        for (int index = 0; index < categories.length; index++) {

        }
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
     * 
     * @param entry
     */
    protected void handleLog(SessionLogEntry entry) {
        List<String> statements = getStatements();

        if (statements != null) {
            statements.add(entry.getMessage());
        }
        if (getOriginal().shouldLog(entry.getLevel(), entry.getNameSpace())) {
            getOriginal().log(entry);
        }
    }

    /**
     * Determines if the
     * 
     * @param level
     * @param category
     * @return
     */
    protected boolean handleShouldLog(int level, String category) {
        if (!getOriginal().shouldLog(level, category)) {
            Set<String> categories = getCategories(level);
            if (categories != null) {
                return categories.contains(category);
            }
        }
        return true;
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
            handleLog((SessionLogEntry) args[0]);
            return null;
        } else if (getStatements() != null && method.getName().equals("shouldLog") && args.length == 2) {
            return handleShouldLog((Integer) args[0], (String) args[1]);
        } else {
            return method.invoke(this.original, args);
        }
    }

    /**
     * Lookup the {@link LogThreadCapture} used to wrap the {@link SessionLog}
     * of the provided session. If none is found create and install one.
     * 
     * @return the enabled SQLCapture for the provided session.
     */
    public static LogThreadCapture enable(Session session) {
        LogThreadCapture handler = null;

        try {
            Object sessionLogHandler = Proxy.getInvocationHandler(session.getSessionLog());
            if (sessionLogHandler instanceof LogThreadCapture) {
                return (LogThreadCapture) sessionLogHandler;
            }
        } catch (IllegalArgumentException iae) {
            // Ignore
        }

        handler = new LogThreadCapture(session.getSessionLog());
        SessionLog proxy = (SessionLog) Proxy.newProxyInstance(session.getPlatform().getConversionManager().getLoader(), new Class[] { SessionLog.class }, handler);
        session.setSessionLog(proxy);

        return handler;
    }

}
