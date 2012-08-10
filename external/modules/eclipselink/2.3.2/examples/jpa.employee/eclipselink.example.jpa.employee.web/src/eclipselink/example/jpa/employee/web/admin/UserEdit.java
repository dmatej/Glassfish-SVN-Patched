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
package eclipselink.example.jpa.employee.web.admin;

import java.util.List;

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.context.FacesContext;
import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletRequest;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.User;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
public class UserEdit {

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    private User user;

    public EmployeeDemoContext getContext() {
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

    public User getUser() {
        if (this.user == null) {
            String id = getUserId();

            if (id != null && !id.isEmpty()) {
                EntityManager em = getContext().createEntityManager();

                try {
                    this.user = em.find(User.class, id);
                } finally {
                    em.close();
                }
            }
        }
        return user;
    }

    public String save() {
        EntityManager em = getContext().createEntityManager();

        try {
            // TODO
            return null;
        } finally {
            em.close();
        }
    }

    public List<Company> getCompanies() {
        EntityManager em = getContext().createEntityManager();

        try {
            return em.createNamedQuery("Company.findAll", Company.class).getResultList();
        } finally {
            em.close();
        }
    }

    /**
     * Retrieve the 'userId' parameter from the {@link FacesContext}
     */
    protected String getUserId() {
        FacesContext context = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) context.getExternalContext().getRequest();
        return myRequest.getParameter("userId");
    }
}
