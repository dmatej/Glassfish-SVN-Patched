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
 *      dclarke - TODO
 ******************************************************************************/
package eclipselink.example.jpa.employee.web.beans;

import eclipselink.example.jpa.employee.model.Project;

public class ViewProjectBean extends BaseManagedBean {

    private Project project;

    /**
     * @return the project
     */
    public Project getProject() {
        return project;
    }

    /**
     * @param project
     *            the project to set
     */
    public void setProject(Project project) {
        this.project = project;
    }

}
