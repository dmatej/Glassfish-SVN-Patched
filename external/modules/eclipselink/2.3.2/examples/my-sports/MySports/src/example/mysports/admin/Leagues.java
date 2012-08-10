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

import java.util.ArrayList;
import java.util.List;

/**
 * Represents the list of leagues returned form the Admin service's REST call as
 * an XML document.
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class Leagues {

    private List<League> leagues = new ArrayList<League>();

    public List<League> getLeagues() {
        return leagues;
    }

}
