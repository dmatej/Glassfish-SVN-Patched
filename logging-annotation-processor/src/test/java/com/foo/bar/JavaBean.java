/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
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

package com.foo.bar;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.glassfish.logging.annotation.LogMessageInfo;
import org.glassfish.logging.annotation.LogMessagesResourceBundle;
import org.glassfish.logging.annotation.LoggerInfo;

/**
 * Class that defines a LoggerInfo and a few LogMessages. 
 *
 */
public class JavaBean {

    // The resourceBundle name to be used for the module's log messages
    @LogMessagesResourceBundle
    public static final String LOGMESSAGES_RB = "com.foo.bar.LogMessages";

    @LoggerInfo(subsystem="EJB", description="Main EJB Logger", publish=true)
    public static final String EJB_LOGGER_NAME = "javax.enterprise.ejb";
    
    private static final Logger EJB_LOGGER =
      Logger.getLogger(EJB_LOGGER_NAME, LOGMESSAGES_RB);

    // Define the log message
    @LogMessageInfo(
        message = "EJB subsystem initialized.",
        comment = "This message indicates that the EJB container initialized successfully.",
        level = "INFO")
    public static final String EJB_SYSTEM_INITIALIZED = "AS-EJB-00001";

    @LogMessageInfo(
        message = "EJB module {0} failed to deploy.",
        comment = "This log message indicates a failure to deploy the given EJB module.",
        level = "SEVERE",
        cause = "Could not deploy an EJB module",
        action = "Verify the deployment descriptor.")
    public static final String EJB_DEPLOYMENT_FAILED = "AS-EJB-00002";

    public void initialize() {
        EJB_LOGGER.log(Level.INFO, EJB_SYSTEM_INITIALIZED);
    }

    public void deploy(String module) {
        EJB_LOGGER.log(Level.SEVERE, EJB_DEPLOYMENT_FAILED, module);
    }
    
}
