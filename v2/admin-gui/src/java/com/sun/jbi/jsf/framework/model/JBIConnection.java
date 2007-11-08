/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
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

package com.sun.jbi.jsf.framework.model;

import com.sun.jbi.jsf.framework.common.JbiConstants;
import com.sun.jbi.jsf.framework.common.XmlUtils;
import org.w3c.dom.Element;

/**
 * JBIConnection.java
 *   List of consumer and provider endpoints
 *
 * @author ylee
 */
public class JBIConnection {

    /** consumer endpoint */
    private JBIEndpoint consumer;
    /** provider endpoint */
    private JBIEndpoint provider;

    
    public JBIConnection(JBIEndpoint consumer, JBIEndpoint provider) {
        this.consumer = consumer;
        this.provider = provider;
    }
    
    public JBIEndpoint getConsumer() {
        return consumer;
    }

    public JBIEndpoint getProvider() {
        return provider;
    }    
    
    public String toString() {
        StringBuffer str = new StringBuffer();
        if ( consumer!=null ) {
            str.append("Consumer: "+consumer.toString());
        }
        if ( provider!=null ) {
            str.append("\nProvider: "+provider.toString());
        }
        return str.toString();
        
    }
    
    public static JBIConnection create(JBIEndpoint consumer, JBIEndpoint provider) {
        return new JBIConnection(consumer,provider);
    }
    
    public static JBIConnection create(Element element) {
        Element consumer = XmlUtils.getChildElement(element, JbiConstants.CONSUMER_TAG);
        JBIEndpoint consumerEndpoint = JBIEndpoint.create(consumer);
        Element provider = XmlUtils.getChildElement(element, JbiConstants.PROVIDER_TAG);
        JBIEndpoint providerEndpoint = JBIEndpoint.create(provider);
        return create(consumerEndpoint,providerEndpoint);
        
    }

}
