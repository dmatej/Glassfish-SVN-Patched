/*
 * @(#)file      ClassLogger.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.16
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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
package com.sun.jdmk.internal;

import java.util.logging.Logger;

public class ClassLogger {

    private final static int NONE=-1,SEVERE=0,WARNING=1,INFO=2,CONFIG=3,
	FINE=4,FINER=5,FINEST=6;
    private static final boolean ok;
    private final String className;
    private final Logger logger;
    private final int level;


    static {
	/* We attempt to work even if we are running in J2SE 1.3, where
	   there is no java.util.logging.  The technique we use here is
	   not strictly portable, but it does work with Sun's J2SE 1.3
	   at least.  This is just a best effort: the Right Thing is for
	   people to use at least J2SE 1.4.  */
	boolean loaded = false;
	try {
	    Class c = java.util.logging.Logger.class;
	    loaded = true;
	} catch (Error e) {
	    // OK.
	    // java.util.logger package is not available in this jvm.
	}
	ok = loaded;
    }

    static {
	/* print out some running environment info. */
	if (ok) {
	    String jmxImp = "Unknown";
	    try {
		final javax.management.MBeanServerDelegate md =
		    new javax.management.MBeanServerDelegate();
		jmxImp = md.getImplementationName()+" "+md.getImplementationVersion();
		
	    } catch (Exception ee) {
		// OK.
		// should not
	    }

	    Logger.getLogger("com.sun.jdmk").logp(java.util.logging.Level.FINE,
		   "ClassLogger",
		   "init",
                   "Java DMK build version [" +
                        com.sun.jdmk.ServiceName.JMX_IMPL_VERSION +"]" +
                   "\n      JMX implementation [" +jmxImp + "]" +
                   "\n      Java version ["+ System.getProperty("java.version") +"]" +
                   "\n      os name [" + System.getProperty("os.name") + "]" +
                   "\n      os arch [" + System.getProperty("os.arch") + "]" +
                   "\n      os version [" + System.getProperty("os.version") + "]");

	}
    }

    public final static String LOGGER_JDMK = "com.sun.jdmk";

    public final static String LOGGER_MBEANSERVER = 
	LOGGER_JDMK + ".mbeanserver";
    public final static String LOGGER_SNMP = 
	LOGGER_JDMK + ".snmp.runtime";
    public final static String LOGGER_PROXY_SNMP = 
	LOGGER_JDMK + ".snmp.proxy";
    public final static String LOGGER_ADAPTOR_SNMP = 
	LOGGER_JDMK + ".snmp.adaptor";
    public final static String LOGGER_ADAPTOR_HTML = 
	LOGGER_JDMK + ".html.adaptor";
    public final static String LOGGER_DISCOVERY = 
	LOGGER_JDMK + ".discovery";
    public final static String LOGGER_NOTIFICATION = 
	LOGGER_JDMK + ".notification";
    public final static String LOGGER_CASCADING = 
	LOGGER_JDMK + ".cascading";
    public final static String LOGGER_LEGACY_CASCADING = 
	LOGGER_JDMK + ".legacy.cascading";
    public final static String LOGGER_MISC = 
	LOGGER_JDMK + ".misc";
    public final static String LOGGER_COMM =
	LOGGER_JDMK + ".comm";
    public final static String LOGGER_CONNECTION_TIMER = 
	LOGGER_COMM + ".timer";

    public final static String LOGGER_LEGACY_RMI = 
	LOGGER_COMM + ".legacy.connector.rmi";
    public final static String LOGGER_LEGACY_HTTP = 
	LOGGER_COMM + ".legacy.connector.http";
    public final static String LOGGER_LEGACY_HTTPS = 
	LOGGER_COMM + ".legacy.connector.https";
    public final static String LOGGER_LEGACY_HEARTBEAT = 
	LOGGER_COMM + ".legacy.connector.heartbeat";
    public final static String LOGGER_LEGACY_SERVER_WRAPPER = 
	LOGGER_COMM + ".legacy.connector.wrapper";
    public final static String LOGGER_LEGACY_CLIENT_WRAPPER = 
	LOGGER_COMM + ".legacy.client.wrapper";

    public static String getClassName(Class clazz) {
	if (clazz == null) return null;
	if (clazz.isArray()) 
	    return getClassName(clazz.getComponentType()) + "[]";
	final String fullname = clazz.getName();
	final int lastpoint   = fullname.lastIndexOf('.');
	final int len         = fullname.length();
	if ((lastpoint < 0) || (lastpoint >= len))
	    return fullname;
	else return fullname.substring(lastpoint+1,len);
    }

    public static String getPackageName(Class clazz) {
	if (clazz == null) return "";
	Package p = clazz.getPackage();
	if (p == null) return "";
	final String pname = p.getName();
	if (pname == null) return "";
	else return pname;
    }

    public ClassLogger(Class clazz) {
	this(getPackageName(clazz),getClassName(clazz));
    }

    public ClassLogger(String subsystem, Class clazz) {
	this(subsystem,getClassName(clazz));
    }

    public ClassLogger(String subsystem, String className) {
	int propLevel = INFO;
	if (ok) {
	    logger    = Logger.getLogger(subsystem);
	    propLevel = NONE;
	} else {
	    logger = null;
	    try {
		// We support these two additional levels in order to be able
		// to filter out the INFO and WARNING traces which are
		// enabled by default.
		//
		if (System.getProperty("LEVEL_SEVERE") != null) 
		    propLevel=SEVERE;
		if (System.getProperty("LEVEL_WARNING") != null) 
		    propLevel=WARNING;

		// This two levels are for backward compatibility with
		// JDMK 5.0. They can still be used when java.util.logging is 
		// not present.
		//
		if (System.getProperty("LEVEL_TRACE") != null) 
		    propLevel=FINER;
		if (System.getProperty("LEVEL_DEBUG") != null) 
		    propLevel=FINEST;
	    } catch (Exception x) {
		// Ok. Probably a security exception.
	    }
	}
	level = propLevel;
	this.className = className;
    }

    public String getClassName() {
	return className;
    }

    public final boolean traceOn() {
	return finerOn();
    }

    public final boolean debugOn() {
	return finestOn();
    }

    public final boolean warningOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.WARNING);
	else return level >= WARNING;
    }

    public final boolean infoOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.INFO);
	else return level >= INFO;
    }

    public final boolean configOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.CONFIG);
	else return level >= CONFIG;
    }

    public final boolean fineOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.FINE);
	else return level >= FINE;
    }

    public final boolean finerOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.FINER);
	else return level >= FINER;
    }

    public final boolean finestOn() {
	if (ok) return logger.isLoggable(java.util.logging.Level.FINEST);
	else return level >= FINEST;
    }

    public final void debug(String func, String msg) {
	finest(func,msg);
    }

    public final void debug(String func, Throwable t) {
	finest(func,t);
    }

    public final void debug(String func, String msg, Throwable t) {
	finest(func,msg,t);
    }

    public final void trace(String func, String msg) {
	finer(func,msg);
    }

    public final void trace(String func, Throwable t) {
	finer(func,t);
    }

    public final void trace(String func, String msg, Throwable t) {
	finer(func,msg,t);
    }

    public final void error(String func, String msg) {
	severe(func,msg);
    }

    public final void error(String func, Throwable t) {
	severe(func,t);
    }

    public final void error(String func, String msg, Throwable t) {
	severe(func,msg,t);
    }

    public final void finest(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINEST, getClassName(), func, 
			msg);
	else if (level >= FINEST) {
	    System.err.println("FINEST: ["+ getClassName() +"] [" + func +"] " 
			       + msg);
	}
    }

    public final void finest(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINEST, getClassName(), func,
			t.toString(), t);
	else if (level >= FINEST) {
	    System.err.println("FINEST: ["+ getClassName() +"] [" + func +"] " 
			       + t);
	    t.printStackTrace();
	}
    }

    public final void finest(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINEST, getClassName(), func, 
			msg,
			t);
	else if (level >= FINEST) {
	    System.err.println("FINEST: ["+ getClassName() +"] [" + func +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void finer(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINER, getClassName(), func, 
			msg);
	else if (level >= FINER) {
	    System.err.println("FINER: ["+ getClassName() +"] [" + func +"] " 
			       + msg);
	}
    }

    public final void finer(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINER, getClassName(), func,
			t.toString(), t);
	else if (level >= FINER) {
	    System.err.println("FINER: ["+ getClassName() +"] [" + func +"] " 
			       + t);
	    t.printStackTrace();
	}
    }

    public final void finer(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINER, getClassName(), func, 
			msg,t);
	else if (level >= FINER) {
	    System.err.println("FINER: ["+ getClassName() +"] [" + func +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void fine(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINE, getClassName(), func, 
			msg);
	else if (level >= FINE) {
	    System.err.println("FINE: ["+ getClassName() +"] [" + func +"] " 
			       + msg);
	}
    }

    public final void fine(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINE, getClassName(), func,
			t.toString(), t);
	else if (level >= FINE) {
	    System.err.println("FINE: ["+ getClassName() +"] [" + func +"] " 
			       + t);
	    t.printStackTrace();
	}
    }

    public final void fine(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.FINE, getClassName(), func, 
			msg,
			t);
	else if (level >= FINE) {
	    System.err.println("FINE: ["+ getClassName() +"] [" + func +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void config(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.CONFIG, getClassName(), func, 
			msg);
	else if (level >= CONFIG) {
	    System.err.println("CONFIG: ["+ getClassName() +"] [" + func +"] " 
			       + msg);
	}
    }

    public final void config(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.CONFIG, getClassName(), func,
			t.toString(), t);
	else if (level >= CONFIG) {
	    System.err.println("CONFIG: ["+ getClassName() +"] [" + func +"] " 
			       + t);
	    t.printStackTrace();
	}
    }

    public final void config(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.CONFIG, getClassName(), func, 
			msg,
			t);
	else if (level >= CONFIG) {
	    System.err.println("CONFIG: ["+ getClassName() +"] [" + func +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void info(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.INFO, getClassName(), func, 
			msg);
	else if (level >= INFO) {
	    System.err.println("INFO: ["+ getClassName() +"] [" + func +"] " 
			       + msg);
	}
    }

    public final void info(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.INFO, getClassName(), func,
			t.toString(), t);
	else if (level >= INFO) {
	    System.err.println("INFO: ["+ getClassName() +"] [" + func +"] " 
			       + t);
	    t.printStackTrace();
	}
    }

    public final void info(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.INFO, getClassName(), func, 
			msg,
			t);
	else if (level >= INFO) {
	    System.err.println("INFO: ["+ getClassName() +"] [" + func +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void warning(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.WARNING, getClassName(), 
			func, msg);
	else if (level >= WARNING) {
	    System.err.println("WARNING: ["+ getClassName() +"] [" + func +
			       "] " + 
			       msg);
	}
    }

    public final void warning(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.WARNING, getClassName(), func,
			t.toString(), t);
	else if (level >= WARNING) {
	    System.err.println("WARNING: ["+ getClassName() +"] [" + func +
			       "] "+ t);
	    t.printStackTrace();
	}
    }

    public final void warning(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.WARNING, getClassName(), func,
			msg,
			t);
	else if (level >= WARNING) {
	    System.err.println("WARNING: ["+ getClassName() +"] [" + func 
			       +"] " 
			       + msg + " " + t);
	    t.printStackTrace();
	}
    }

    public final void severe(String func, String msg) {
	if (ok)
	    logger.logp(java.util.logging.Level.SEVERE, getClassName(), func, 
			msg);
	else
	    System.err.println("SEVERE: ["+ getClassName() +"] [" + func +
			       "] " +
			       msg);
    }

    public final void severe(String func, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.SEVERE, getClassName(), func,
			t.toString(), t);
	else {
	    System.err.println("SEVERE: ["+ getClassName() +"] [" + func +
			       "] " + t);
	    t.printStackTrace();
	}
    }

    public final void severe(String func, String msg, Throwable t) {
	if (ok)
	    logger.logp(java.util.logging.Level.SEVERE, getClassName(), func,
			msg,
			t);
	else {
	    System.err.println("SEVERE: ["+ getClassName() +"] [" + func +
			       "] " + 
			       msg + " " + t);
	    t.printStackTrace();
	}
    }
}
