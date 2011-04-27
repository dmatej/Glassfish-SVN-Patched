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

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;

import javax.servlet.ServletContext;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URI;
import java.net.URL;
import java.net.URLConnection;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

/**
 * @author Sanjeeb.Sahoo@Sun.COM
 */
public class WebAppBundle implements WABDeploymentEventHandler.Callback {
    enum State {
        DEPLOYING, DEPLOYED, UNDEPLOYING, UNDEPLOYED, FAILED
    }

    BundleContext context;

    Bundle b;

    String contextPath;

    State state;

    CountDownLatch deploymentSignal = new CountDownLatch(1);

    public WebAppBundle(BundleContext context, Bundle b) {
        this.context = context;
        this.b = b;
    }

    public ServletContext deploy(long timeout, TimeUnit timeUnit) throws InterruptedException, BundleException {
        WABDeploymentEventHandler eventHandler = new WABDeploymentEventHandler(context, b, this);
        b.start(Bundle.START_TRANSIENT);
        deploymentSignal.await(timeout, timeUnit);
        if (State.DEPLOYED.equals(state)) {
            return (ServletContext) context.getService(context.getServiceReference(ServletContext.class.getName()));
        }
        throw new RuntimeException("Deployment failure. Check server.log for details");
    }

    public void undeploy() throws BundleException {
        b.stop(Bundle.STOP_TRANSIENT);
    }

    public String getResponse(String path) throws IOException {
        URL servlet = new URL("http", getHost(), getPort(), contextPath + path);
        URLConnection yc = servlet.openConnection();
        BufferedReader in = new BufferedReader(
                new InputStreamReader(
                        yc.getInputStream()));

        StringBuilder sb = new StringBuilder();
        String inputLine;
        while ((inputLine = in.readLine()) != null) {
            sb.append(inputLine);
        }
        in.close();
        return sb.toString();
    }

    private String getHost() {
        return "localhost";
    }

    private int getPort() {
        return 8080;
    }

    @Override
    public void deploying() {
        state = State.DEPLOYING;
    }

    @Override
    public void deployed(String contextPath) {
        state = State.DEPLOYED;
        this.contextPath = contextPath;
        deploymentSignal.countDown();
    }

    @Override
    public void undeploying() {
        state = State.UNDEPLOYING;
    }

    @Override
    public void undeployed() {
        state = State.UNDEPLOYED;
    }

    @Override
    public void failed(Throwable throwable, String collision, Long[] collisionBundleIds) {
        state = State.FAILED;
        deploymentSignal.countDown();
    }
}