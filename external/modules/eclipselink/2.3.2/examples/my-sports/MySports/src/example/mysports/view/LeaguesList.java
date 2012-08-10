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
import javax.inject.Inject;

import example.mysports.MySportsConfig;
import example.mysports.admin.Leagues;

/**
 * Return list of available Leagues from JAX-RS call to MySports Admin app.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
@ManagedBean
public class LeaguesList {

    private Leagues leagues;

    private MySportsConfig config;

    public MySportsConfig getConfig() {
        return config;
    }

    @Inject
    public void setConfig(MySportsConfig config) {
        this.config = config;
    }

    public Leagues getLeagues() {
        if (this.leagues == null) {
            this.leagues = getConfig().getAdminConnector().getLeagues();
        }
        return this.leagues;
    }

}
