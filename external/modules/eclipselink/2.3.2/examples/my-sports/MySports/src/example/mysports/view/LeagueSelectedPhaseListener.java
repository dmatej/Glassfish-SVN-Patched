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
import javax.faces.application.NavigationHandler;
import javax.faces.context.FacesContext;
import javax.faces.event.PhaseEvent;
import javax.faces.event.PhaseId;
import javax.faces.event.PhaseListener;
import javax.servlet.http.HttpServletResponse;

/**
 * {@link PhaseListener} responsible for ensuring that if there is no current
 * league selected that the application always returns to the landing page
 * requiring a league to be selected. Also avoid any page caching in browsers.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.1
 */
public class LeagueSelectedPhaseListener implements PhaseListener {
    private static final long serialVersionUID = 1L;

    private LeagueRepositoryBean getRepositoryBean(FacesContext context) {
        ELContext elContext = context.getELContext();
        return (LeagueRepositoryBean) elContext.getELResolver().getValue(elContext, null, "leagueRepositoryBean");
    }

    public PhaseId getPhaseId() {
        return PhaseId.RESTORE_VIEW;
    }

    public void beforePhase(PhaseEvent event) {
        FacesContext facesContext = event.getFacesContext();
        HttpServletResponse response = (HttpServletResponse) facesContext.getExternalContext().getResponse();
        response.addHeader("Pragma", "no-cache");
        response.addHeader("Cache-Control", "no-cache");
        // Stronger according to blog comment below that references HTTP spec
        response.addHeader("Cache-Control", "no-store");
        response.addHeader("Cache-Control", "must-revalidate");
        // some date in the past
        response.addHeader("Expires", "Mon, 8 Aug 2006 10:00:00 GMT");
    }

    public void afterPhase(PhaseEvent event) {
        FacesContext fc = event.getFacesContext();

        if (!fc.getViewRoot().getViewId().contains("index.xhtml") && !getRepositoryBean(fc).getContext().hasLeague()) {
            NavigationHandler nh = fc.getApplication().getNavigationHandler();
            nh.handleNavigation(fc, null, LeagueRepositoryBean.SELECT_LEAGUE_PAGE);
        }
    }

}