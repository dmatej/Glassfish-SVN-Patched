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
 *      dclarke - TODO 
 ******************************************************************************/
package eclipselink.example.jpa.employee.web.user;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ViewScoped;
import javax.persistence.EntityManager;

@ManagedBean
@ViewScoped
public class EmployeeDelete extends EmployeeView {
    private static final long serialVersionUID = 1L;

    private String errorMessage;

    public String getErrorMessage() {
        return errorMessage;
    }

    public String delete() {
        EntityManager em = getContext().createEntityManager();

        try {
            return null;
        } finally {
            em.close();
        }
    }

}
