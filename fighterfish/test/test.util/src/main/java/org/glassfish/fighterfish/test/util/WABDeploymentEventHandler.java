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
import org.osgi.framework.ServiceRegistration;
import org.osgi.service.event.Event;
import org.osgi.service.event.EventConstants;
import org.osgi.service.event.EventHandler;

import java.util.Hashtable;

/**
 * Listens to OSGi Event Admin Service generated events and calls back a registered handler
 */
public class WABDeploymentEventHandler implements EventHandler {
    public interface Callback {
        void deploying();

        void deployed(String contextPath);

        void undeploying();

        void undeployed();

        void failed(Throwable throwable, String collision, Long[] collisionBundleIds);
    }

    private Callback callback;
    private ServiceRegistration registration;


    /**
     * @param context  BundleContext used to register the service
     * @param b        Bundle whose deployment related events we are interested in
     * @param callback object that will be called back when appropriate events are received
     */
    public WABDeploymentEventHandler(BundleContext context, Bundle b, Callback callback) {
        this.callback = callback;
        String[] topics = new String[]{"org/osgi/service/web/*"};
        Hashtable ht = new Hashtable();
        ht.put(EventConstants.EVENT_TOPIC, topics);
        final String filterString = "(" + EventConstants.BUNDLE_ID + "=" + b.getBundleId() + ")";
        ht.put(EventConstants.EVENT_FILTER, filterString);
        registration = context.registerService(EventHandler.class.getName(), this, ht);
    }

    @Override
    public void handleEvent(Event event) {
        final String topic = event.getTopic();
        System.out.println(topic);
        System.out.println("event = " + event);
        if ("org/osgi/service/web/DEPLOYING".equals(topic)) {
            callback.deploying();
        } else if ("org/osgi/service/web/DEPLOYED".equals(topic)) {
            String contextPath = (String) event.getProperty("context.path");
            callback.deployed(contextPath);
        } else if ("org/osgi/service/web/FAILED".equals(topic)) {
            Throwable throwable = (Throwable) event.getProperty("exception");
            String collision = (String) event.getProperty("collision");
            Long[] collisionBundleIds = (Long[]) event.getProperty("collision.bundles");
            callback.failed(throwable, collision, collisionBundleIds);
        } else if ("org/osgi/service/web/UNDEPLOYING".equals(topic)) {
            callback.undeploying();
        } else if ("org/osgi/service/web/UNDEPLOYED".equals(topic)) {
            callback.undeployed();
        }
    }

    /**
     * Stop listening for events.
     */
    public void stop() {
        registration.unregister();
    }
}