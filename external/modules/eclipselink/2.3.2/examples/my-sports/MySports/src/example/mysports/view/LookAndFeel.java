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

import javax.el.ELContext;
import javax.faces.bean.ManagedBean;
import javax.faces.context.FacesContext;
import javax.inject.Inject;

import example.mysports.MySportsConfig;

/**
 * JSF managed bean used to provide custom the look and feel of the UI in a
 * league specific manner. The CSS and league logo are retrieved from the admin
 * application
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@ManagedBean
public class LookAndFeel {

    private MySportsConfig config;
    
    public MySportsConfig getConfig() {
        return config;
    }

    @Inject
    public void setConfig(MySportsConfig config) {
        this.config = config;
    }

    private LeagueRepositoryBean getRepositoryBean() {
        FacesContext context = FacesContext.getCurrentInstance();
        ELContext elContext = context.getELContext();
        return (LeagueRepositoryBean) elContext.getELResolver().getValue(elContext, null, "leagueRepositoryBean");
    }

    private String getAdminServerURL() {
        return getConfig().getAdminURL(FacesContext.getCurrentInstance().getExternalContext()) + config.getAdminContext();
    }

    public String getCss() {
        return getAdminServerURL() + "/" + getRepositoryBean().getContext().getLeagueId() + ".css";
    }

    public String getLogo() {
        return getAdminServerURL() + "/" + getRepositoryBean().getContext().getLeagueId() + ".png";
    }

    public boolean isMultitenant() {
        return getConfig().isMultitenant();
    }

}
