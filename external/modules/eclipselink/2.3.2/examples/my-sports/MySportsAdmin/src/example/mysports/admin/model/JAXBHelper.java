/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.admin.model;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.persistence.jaxb.JAXBContextFactory;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class JAXBHelper {
    private static final String MAPPING_FILE = "META-INF/eclipselink-oxm.xml";
    
    public static JAXBContext createContext() throws JAXBException{
        ClassLoader cl = League.class.getClassLoader();
        InputStream bindings = cl.getResourceAsStream(MAPPING_FILE);
        try {
            Map<String, Object> props = new HashMap<String, Object>(1);
            props.put(JAXBContextFactory.ECLIPSELINK_OXM_XML_KEY, bindings);
            return JAXBContext.newInstance(new Class[] { League.class }, props);
        } finally {
            try {
                bindings.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }

    }
}
