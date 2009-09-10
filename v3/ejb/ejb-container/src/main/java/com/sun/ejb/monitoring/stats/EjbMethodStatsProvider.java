/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
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
package com.sun.ejb.monitoring.stats;

import java.lang.reflect.Method;
import java.util.List;
import java.util.ArrayList;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.external.probe.provider.annotations.*;
import org.glassfish.external.statistics.*;
import org.glassfish.external.statistics.impl.*;
import org.glassfish.gmbal.*;
import com.sun.enterprise.admin.monitor.stats.MutableTimeStatistic;
import com.sun.enterprise.admin.monitor.stats.MutableTimeStatisticImpl;

import com.sun.ejb.containers.EjbContainerUtilImpl;

/**
 * Event listener for the Ejb monitoring events. Used by the probe framework 
 * to collect and display the data.
 *
 * @author Marina Vatkina
 */
// TODO: find the right names
// v2: com.sun.appserv:application=MEjbApp,name=getEJBHome,type=bean-method,category=monitor,ejb-module=mejb_jar,server=server,stateless-session-bean=MEJBBean
// v3: amx:pp=/mon/server-mon[server],type=bean-method-mon,name=??????????
@AMXMetadata(type="bean-method-mon", group="monitoring", isSingleton=false)
@ManagedObject
@Description("Ejb Method Statistics")
public class EjbMethodStatsProvider {

    private CountStatisticImpl executionStat = new CountStatisticImpl(
            "ExecutionTime", "Milliseconds", null);

    private CountStatisticImpl invocationStat = new CountStatisticImpl(
            "TotalNumInvocations", "count", null);

    private CountStatisticImpl errorStat = new CountStatisticImpl(
            "TotalNumErrors", "count", null);

    private CountStatisticImpl successStat = new CountStatisticImpl(
                "TotalNumSuccess", "count", null);

    private MutableTimeStatisticImpl methodStat = null;

    private static ThreadLocal  execThreadLocal = new ThreadLocal();
    private Method m = null;
    private boolean registered = false;

    EjbMethodStatsProvider (Method m) {
        this.m = m;

        long now = System.currentTimeMillis();
        methodStat = new MutableTimeStatisticImpl(
                new com.sun.enterprise.admin.monitor.stats.TimeStatisticImpl(
                0, 0, 0, 0,
                "MethodStatistic", "", "", now, now));
    }

    @ManagedAttribute(id="methodstatistic")
    @Description("Number of times the operation is called; total time spent during invocation, and so on.")
    public TimeStatistic getMethodStatistic() {
        return null; //methodStat.modifiableView();
    }

    @ManagedAttribute(id="totalnumerrors")
    @Description("Number of times the method execution resulted in an exception")
    public CountStatistic getTotalNumErrors() {
        return errorStat;
    }

    @ManagedAttribute(id="totalnumsuccess")
    @Description("Number of times the method successfully executed")
    public CountStatistic getTotalNumSuccess() {
        return successStat;
    }

    @ManagedAttribute(id="executiontime")
    @Description("Time (ms) spent executing method for the last successful/unsuccessful " 
        + "attempt to execute the operation")
    public CountStatistic getTotalExecutionTime() {
        return executionStat;
    }

    void registered() {
        registered = true;
    }

    void unregistered() {
        registered = false;
    }

    boolean isRegistered() {
        return registered;
    }

    void methodStart() {
        List list = (ArrayList) execThreadLocal.get();
        if (list == null) {
            list = new ArrayList(5);
            execThreadLocal.set(list);
        }
        list.add(new Long(System.currentTimeMillis()));
        invocationStat.increment();
    }

    void methodEnd(boolean success) {
        List list = (ArrayList) execThreadLocal.get();
        if ( (list != null) && (list.size() > 0) ) {
            int index = list.size();
            Long startTime = (Long) list.remove(index-1);
            if (success) {
                successStat.increment();
            } else {
                errorStat.increment();
            }
            if (startTime != null) {
                long diff = System.currentTimeMillis() - startTime.longValue();
                executionStat.setCount(diff);
                methodStat.incrementCount(diff);
            }
        }
    }
}
