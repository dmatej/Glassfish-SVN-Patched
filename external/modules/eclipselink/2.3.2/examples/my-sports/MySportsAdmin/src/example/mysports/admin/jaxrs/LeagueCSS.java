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
package example.mysports.admin.jaxrs;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;

import example.mysports.admin.CSSLoader;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
@Path("css")
public class LeagueCSS {

    @GET
    @Produces("text/plain")
    public String getCSS(@QueryParam("league") String leagueId) {
        return CSSLoader.getCSS(CSSLoader.class.getClassLoader(), leagueId);
    }

}