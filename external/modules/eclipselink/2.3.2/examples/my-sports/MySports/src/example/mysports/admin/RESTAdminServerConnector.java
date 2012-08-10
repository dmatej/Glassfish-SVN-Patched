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
package example.mysports.admin;

import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.core.MediaType;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;

import org.eclipse.persistence.internal.jpa.EntityManagerFactoryProvider;
import org.eclipse.persistence.jaxb.JAXBContextFactory;
import org.eclipse.persistence.sessions.factories.SessionManager;

import example.mysports.MySportsConfig;

/**
 * Utility class that provides access to the admin server using JAX-RS calls.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class RESTAdminServerConnector implements AdminServerConnector {

    private MySportsConfig config;

    public MySportsConfig getConfig() {
        return config;
    }

    public void setConfig(MySportsConfig config) {
        this.config = config;
    }

    public Leagues getLeagues() {
        SessionManager.getManager().destroyAllSessions();
        EntityManagerFactoryProvider.getEmSetupImpls().clear();
        
        InputStream in = null;

        try {
            in = open(null, null);
            Leagues leagues = (Leagues) RESTAdminServerConnector.getContext().createUnmarshaller().unmarshal(in);
            return leagues;
        } catch (IOException e) {
            throw new RuntimeException("Failure to retieve Leagues", e);
        } catch (JAXBException e) {
            throw new RuntimeException("Failure to unmarshal Leagues", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                }
            }
        }
    }

    public League getLeague(String leagueId) {
        InputStream in = null;

        try {
            in = open(leagueId, null);
            return (League) RESTAdminServerConnector.getContext().createUnmarshaller().unmarshal(in);
        } catch (IOException e) {
            throw new RuntimeException("Failure to retieve League", e);
        } catch (JAXBException e) {
            throw new RuntimeException("Failure to unmarshal League", e);
        } finally {
            if (in != null) {
                try {
                    in.close();
                } catch (IOException e) {
                }
            }
        }
    }

    /**
     * Construct a URL to access the admin server for a specified league and
     * resource result type.
     * 
     * @param leagueId
     *            represents an expected league or null if requesting all
     *            leagues.
     * @param result
     *            result resource type being requested. If null with a valid
     *            league identifier then the league is being accessed. If
     *            leagueId is null this should be null as well.
     */
    private String buildURL(String leagueId, String result) {
        String urlString = "http://localhost:8080" + getConfig().getAdminContext();

        if (leagueId != null) {
            urlString = urlString + "/" + leagueId;
        }
        if (result != null) {
            urlString = urlString + "/" + result.toLowerCase();
        }

        return urlString;
    }

    private InputStream open(String leagueId, String result) throws IOException {
        URL url = new URL(buildURL(leagueId, result));
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestProperty("Accept", MediaType.APPLICATION_XML);

        return connection.getInputStream();
    }

    /**
     * Cached {@link JAXBContext} for marshaling {@link Leagues} and
     * {@link League} from the admin server requests.
     */
    private static JAXBContext jaxbContext;

    public static JAXBContext getContext() throws JAXBException {
        if (jaxbContext == null) {
            Map<String, Object> props = new HashMap<String, Object>(1);
            props.put(JAXBContextFactory.ECLIPSELINK_OXM_XML_KEY, "example/mysports/admin/leagues-oxm.xml");
            jaxbContext = JAXBContextFactory.createContext(new Class[] { Leagues.class }, props);
        }
        return jaxbContext;
    }

    public InputStream getCss(String leagueId) throws IOException {
        return open(leagueId, "css");
    }

    public String getOrmURL(String leagueId) {
        return buildURL(leagueId, "orm");
    }

    public String getOxmURL(String leagueId) {
        return buildURL(leagueId, "oxm");
    }

    public InputStream getLogo(String leagueId) throws IOException {
        return open(leagueId, "logo");
    }

}
