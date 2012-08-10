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
package example.mysports.tests.admin;

import java.io.InputStream;
import java.net.URI;
import java.util.List;

import javax.persistence.EntityManagerFactory;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.PathSegment;
import javax.ws.rs.core.UriBuilder;
import javax.ws.rs.core.UriInfo;

import example.mysports.MySportsConfig;
import example.mysports.admin.AdminServerConnector;
import example.mysports.admin.League;
import example.mysports.admin.Leagues;
import example.mysports.admin.jaxrs.HostedLeagues;
import example.mysports.admin.model.HostedLeague;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class MockAdminServerConnector implements AdminServerConnector {

    private HostedLeagues leaguesBean;

    private MySportsConfig config;
    

    public void setEMF(EntityManagerFactory emf) {
        this.leaguesBean = new HostedLeagues();
        this.leaguesBean.setEmf(emf);
        this.leaguesBean.setContext(new MockUriInfo());
    }
    
    @Override
    public MySportsConfig getConfig() {
        return this.config;
    }

    @Override
    public void setConfig(MySportsConfig config) {
        this.config = config;
    }

    public Leagues getLeagues() {
        List<HostedLeague> allLeagues = this.leaguesBean.allLeagues();

        Leagues result = new example.mysports.admin.Leagues();
        for (HostedLeague hl : allLeagues) {
            League l = new League();
            l.setId(hl.getId());
            l.setName(hl.getName());
            l.setVersion(hl.getVersion());
            result.getLeagues().add(l);
        }

        return result;
    }

    public League getLeague(String leagueId) {
        HostedLeague hl = this.leaguesBean.getLeague(leagueId);

        League l = null;

        if (hl != null) {
            l = new League();
            l.setId(hl.getId());
            l.setName(hl.getName());
            l.setVersion(hl.getVersion());
        }
        return l;
    }

    public InputStream getCss(String leagueId) {
        throw new RuntimeException("NOT YET IMPLEMENTED");
    }

    public InputStream getLogo(String leagueId) {
        throw new RuntimeException("NOT YET IMPLEMENTED");
    }

    public String getOrmURL(String leagueId) {
        return null;
    }

    public String getOxmURL(String leagueId) {
        return null;
    }

    public EntityManagerFactory getEMF() {
        return this.leaguesBean.getEmf();
    }

    class MockUriInfo implements UriInfo {

        @Override
        public URI getAbsolutePath() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public UriBuilder getAbsolutePathBuilder() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public URI getBaseUri() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public UriBuilder getBaseUriBuilder() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<Object> getMatchedResources() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<String> getMatchedURIs() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<String> getMatchedURIs(boolean decode) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public String getPath() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public String getPath(boolean decode) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public MultivaluedMap<String, String> getPathParameters() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public MultivaluedMap<String, String> getPathParameters(boolean decode) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<PathSegment> getPathSegments() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public List<PathSegment> getPathSegments(boolean decode) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public MultivaluedMap<String, String> getQueryParameters() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public MultivaluedMap<String, String> getQueryParameters(boolean decode) {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public URI getRequestUri() {
            // TODO Auto-generated method stub
            return null;
        }

        @Override
        public UriBuilder getRequestUriBuilder() {
            // TODO Auto-generated method stub
            return null;
        }

    }

}
