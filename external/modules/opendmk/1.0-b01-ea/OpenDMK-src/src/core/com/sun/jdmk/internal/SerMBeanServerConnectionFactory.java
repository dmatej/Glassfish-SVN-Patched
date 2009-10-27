/*
 * @(#)file      SerMBeanServerConnectionFactory.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.6
 * @(#)date      07/04/04
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
 *
 */
package com.sun.jdmk.internal;

import java.io.Serializable;
import java.io.IOException;

import java.util.Map;

import javax.management.NotificationEmitter;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ListenerNotFoundException;
import javax.management.MBeanServerConnection;

import javax.management.remote.JMXServiceURL;

import com.sun.jdmk.remote.cascading.MBeanServerConnectionFactory;

import com.sun.jdmk.internal.ReconnectMBeanServerConnectionFactory;


public class SerMBeanServerConnectionFactory implements Serializable, 
							MBeanServerConnectionFactory {
    private static final long serialVersionUID = -3517891643475463938L;
    transient ReconnectMBeanServerConnectionFactory factory;
    JMXServiceURL url;
    Map map;
    long envelopTime;
    long reconnectPeriod;
    
    public SerMBeanServerConnectionFactory(JMXServiceURL url,
					   Map map,
					   long envelopTime,
					   long reconnectPeriod) {
	this.url = url;
	this.map = map;
	this.envelopTime = envelopTime;
	this.reconnectPeriod = reconnectPeriod;
    }
    
    private ReconnectMBeanServerConnectionFactory getFactory() 
	throws IOException {
	if(factory == null) {
	    factory = (ReconnectMBeanServerConnectionFactory)
		ReconnectMBeanServerConnectionFactory.newInstance(url,
								  map,
								  envelopTime,
								  reconnectPeriod);
	}
	return factory;
    }
    
    private void readObject(java.io.ObjectInputStream in)
	throws IOException, ClassNotFoundException {
	in.defaultReadObject();
    }
    
    public void addConnectionNotificationListener(NotificationListener listener, NotificationFilter filter, Object handback) {
	try {
	    getFactory().addConnectionNotificationListener(listener,
							   filter,
							   handback);
	}catch(IOException e) {
	    //
	}
    }
    
    public String getConnectionId() throws IOException {
	return getFactory().getConnectionId();
    }
    
    public MBeanServerConnection getMBeanServerConnection() throws IOException{
	return getFactory().getMBeanServerConnection();
    }
     
    public void removeConnectionNotificationListener(NotificationListener listener) throws ListenerNotFoundException {
	try {
	    getFactory().removeConnectionNotificationListener(listener);
	}catch(IOException e) {
	    //
	}
    }
    
    public void removeConnectionNotificationListener(NotificationListener l, NotificationFilter f, Object handback) throws ListenerNotFoundException {
	try {
	    getFactory().removeConnectionNotificationListener(l, f, handback);
	}catch(IOException e) {
	    //
	}  
    }
}
