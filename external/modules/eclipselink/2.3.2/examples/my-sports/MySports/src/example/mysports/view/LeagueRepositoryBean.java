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
package example.mysports.view;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;

import example.mysports.MySportsConfig;
import example.mysports.persistence.TenantContext;
import example.mysports.persistence.TenantPersistence;
import example.mysports.service.LeagueRepository;

/**
 * JSF session scoped managed bean which looks after access to the stateful
 * {@link LeagueRepository} EJB.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean
@SessionScoped
public class LeagueRepositoryBean {

    private TenantContext context;

    protected final static String SELECT_LEAGUE_PAGE = "index?faces-redirect=true";

    public TenantContext getContext() {
        return context;
    }

    @Inject
    public void setContext(TenantContext context) {
        this.context = context;
    }

    public String setLeague() {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) facesContext.getExternalContext().getRequest();
        String leagueId = myRequest.getParameter("league");
        String versionString = myRequest.getParameter("version");
        
        long version = 0;
        if (versionString != null) {
            version = Long.valueOf(version);
            getContext().getProperties().put("VERSION", version);
        }
        
        getContext().setLeagueId(leagueId);

        // Refresh metadata if the EMF exists and its version has changed
        if (versionString != null) {
            TenantPersistence.refreshEntityManagerFactory(MySportsConfig.PU_NAME, leagueId, version, getContext().getProperties());
        }

        return getContext().hasLeague() ? ViewLeague.PAGE : SELECT_LEAGUE_PAGE;
    }

    public String clearLeague() {
        getContext().setLeagueId(null);
        return SELECT_LEAGUE_PAGE;
    }

    public String getLeagueId() {
        return getContext().getLeagueId();
    }

}
