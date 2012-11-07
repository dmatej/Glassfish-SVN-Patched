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


package org.glassfish.fighterfish.test.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.PrintWriter;
import java.lang.management.*;
import java.util.Date;

/**
 * This object represents the current stack traces of all threads in the system - very much similar to
 * the output of jstack command line tool. Its {@link #toString()} returns the stack traces of all the threads
 * by calling underlying {@link ThreadMXBean}.
 *
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class JStack {
    @Override
    public String toString() {
        ThreadMXBean threadMXBean = ManagementFactory.getThreadMXBean();
        return getAllStack(threadMXBean.dumpAllThreads(true, true));
    }

    private static String getAllStack(ThreadInfo[] tis) {
        if (tis == null) return "null";
        StringBuilder b = new StringBuilder("[");
        for (ThreadInfo ti : tis) {
            b.append("\n [" + getStack(ti) + " ]");
            if (ti != tis[tis.length -1]) b.append(",");
        }
        b.append("\n]");
        return b.toString();
    }

    private static String getStack(ThreadInfo ti) {
        /*
         * This method has been largely copied from ThreadInfo.java as toString() of ThreadInfo
         */
        StringBuilder sb = new StringBuilder("\"" + ti.getThreadName() + "\"" +
                                             " Id=" + ti.getThreadId() + " " +
                                             ti.getThreadState());
        if (ti.getLockName() != null) {
            sb.append(" on " + ti.getLockName());
        }
        if (ti.getLockOwnerName() != null) {
            sb.append(" owned by \"" + ti.getLockOwnerName() +
                      "\" Id=" + ti.getLockOwnerId());
        }
        if (ti.isSuspended()) {
            sb.append(" (suspended)");
        }
        if (ti.isInNative()) {
            sb.append(" (in native)");
        }
        sb.append('\n');
        int i = 0;
        StackTraceElement[] stackTrace = ti.getStackTrace();
        for (; i < stackTrace.length; i++) {
            StackTraceElement ste = stackTrace[i];
            sb.append("\t\tat " + ste.toString());
            sb.append('\n');
            if (i == 0 && ti.getLockInfo() != null) {
                Thread.State ts = ti.getThreadState();
                switch (ts) {
                    case BLOCKED:
                        sb.append("\t-  blocked on " + ti.getLockInfo());
                        sb.append('\n');
                        break;
                    case WAITING:
                        sb.append("\t-  waiting on " + ti.getLockInfo());
                        sb.append('\n');
                        break;
                    case TIMED_WAITING:
                        sb.append("\t-  waiting on " + ti.getLockInfo());
                        sb.append('\n');
                        break;
                    default:
                }
            }

            for (MonitorInfo mi : ti.getLockedMonitors()) {
                if (mi.getLockedStackDepth() == i) {
                    sb.append("\t-  locked " + mi);
                    sb.append('\n');
                }
            }
       }

       LockInfo[] locks = ti.getLockedSynchronizers();
       if (locks.length > 0) {
           sb.append("\n\tNumber of locked synchronizers = " + locks.length);
           sb.append('\n');
           for (LockInfo li : locks) {
               sb.append("\t- " + li);
               sb.append('\n');
           }
       }
       sb.append('\n');
       return sb.toString();

    }

    public void printStackTrace(File f) {
        final String s = toString();
        try {
            System.out.println("JStack written out to " + f.getAbsolutePath());
            new PrintWriter(new FileOutputStream(f)).print("Stack trace generated at " + new Date() + "\n" + s);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
        }
    }

    public static void main(String[] args) {
        new Thread() {
            {
                setDaemon(false);
            }
            @Override
            public void run() {
                synchronized (this) {
                    try {
                        wait();
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e); // TODO(Sahoo): Proper Exception Handling
                    }
                }
            }
        }.start();
        System.out.println(new JStack());
    }
}
