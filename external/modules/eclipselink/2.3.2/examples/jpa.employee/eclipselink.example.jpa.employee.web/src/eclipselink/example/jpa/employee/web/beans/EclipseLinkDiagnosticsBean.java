/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - EclipseLink Employee JSF-EJB-JPA Example
 ******************************************************************************/
package eclipselink.example.jpa.employee.web.beans;

import java.util.Collection;

/**
 * TODO 
 * 
 * @author dclarke
 */
public class EclipseLinkDiagnosticsBean extends BaseManagedBean {

    private boolean enabled = false;

    private boolean displaySQL = true;

    public boolean isDisplaySQL() {
        return displaySQL;
    }

    public void setDisplaySQL(boolean displaySQL) {
        this.displaySQL = displaySQL;
    }

    public Collection<String> getSqlStatements() {
        return null;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String toggleDiagnostics() {
        this.enabled = !this.enabled;
        return "";
    }

}
