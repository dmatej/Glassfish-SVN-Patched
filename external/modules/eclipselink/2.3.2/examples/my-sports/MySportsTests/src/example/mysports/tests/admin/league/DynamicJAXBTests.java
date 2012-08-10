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
package example.mysports.tests.admin.league;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.List;

import javax.xml.bind.JAXBException;

import junit.framework.Assert;

import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.jaxb.dynamic.DynamicJAXBContext;
import org.eclipse.persistence.jaxb.dynamic.DynamicJAXBContextFactory;
import org.junit.Test;

/**
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class DynamicJAXBTests {

    @Test
    public void verifyContext() throws Exception {
        DynamicJAXBContext jaxbContext = createContext();

        Assert.assertNotNull(jaxbContext);
    }

    @Test
    public void verifyReadFromAdminServer() throws Exception {
        DynamicJAXBContext jaxbContext = createContext();
        URL url = new URL("http://localhost:8080/MySportsAdmin/rest/leagues");

        DynamicEntity allLeagues =  (DynamicEntity) jaxbContext.createUnmarshaller().unmarshal(url);
        
        Assert.assertNotNull(allLeagues);
        List<DynamicEntity> leagues = allLeagues.<List<DynamicEntity>>get("league");
        Assert.assertNotNull(leagues);
        
        for (DynamicEntity league: leagues) {
            System.out.println("League(" + league.<String>get("id") + ", " + league.get("value") + ")");
        }
    }

    private DynamicJAXBContext createContext() throws IOException, JAXBException {
        URL url = new URL("http://localhost:8080/MySportsAdmin/xsds/leagues.xsd");
        InputStream in = url.openStream();

        try {
            return DynamicJAXBContextFactory.createContextFromXSD(in, null, null, null);
        } finally {
            in.close();
        }

    }
}
