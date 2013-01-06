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

package org.glassfish.fighterfish.test.app16.msgproducer;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.Constants;
import org.osgi.service.cm.ConfigurationException;
import org.osgi.service.cm.ManagedService;

import javax.jms.*;
import javax.naming.InitialContext;
import javax.naming.NamingException;

import java.util.Dictionary;
import java.util.Properties;

/**
 * @author sanjeeb.sahoo@oracle.com
 * 
 */
public class TestApp16ProducerActivator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        System.out
                .println("Message producer started - waiting to be configured with topic name");
        Properties props = new Properties();
        final String pkgName = getClass().getPackage().getName();
        props.put(Constants.SERVICE_PID, pkgName);
        context.registerService(ManagedService.class.getName(),
                new ManagedService() {
                    public void updated(Dictionary properties)
                            throws ConfigurationException {
                        if (properties != null) {
                            String destinationName = (String) properties
                                    .get(pkgName + ".Destination");
                            String connectionFactoryName = (String) properties
                                    .get(pkgName + ".ConnectionFactory");
                            int noOfMsgs = Integer.valueOf((String) properties
                                    .get(pkgName + ".NoOfMsgs"));
                            sendMessage(connectionFactoryName, destinationName,
                                    noOfMsgs);
                        }
                    }
                }, props);
    }

    /**
     * Create connection. Create session from connection; false means session is
     * not transacted. Create producer and text message. Send messages, varying
     * text slightly. Send end-of-messages message. Finally, close connection.
     */
    private void sendMessage(String connectionFactoryName,
            String destinationName, int noOfMsgs) {

        Connection connection = null;
        try {
            InitialContext ctx = new InitialContext();

            ConnectionFactory connectionFactory = (ConnectionFactory) ctx
                    .lookup(connectionFactoryName);

            connection = connectionFactory.createConnection();

            Session session = connection.createSession(false,
                    Session.AUTO_ACKNOWLEDGE);

            Destination dest = (Destination) ctx.lookup(destinationName);
            MessageProducer producer = session.createProducer(dest);
            TextMessage message = session.createTextMessage();

            for (int i = 0; i < noOfMsgs; i++) {
                message.setText("This is message " + (i + 1));
                System.out.println("Sending message: " + message.getText());
                producer.send(message);
            }

            /*
             * Send a non-text control message indicating end of messages.
             */
            producer.send(session.createMessage());
        } catch (JMSException e) {
            System.err.println("Exception occurred: " + e.toString());
        } catch (NamingException e) {
            System.err.println("Exception occurred: " + e.toString());
        } finally {
            if (connection != null) {
                try {
                    connection.close();
                } catch (JMSException e) {
                }
            }
        }
    }

    public void stop(BundleContext context) throws Exception {
    }

}
