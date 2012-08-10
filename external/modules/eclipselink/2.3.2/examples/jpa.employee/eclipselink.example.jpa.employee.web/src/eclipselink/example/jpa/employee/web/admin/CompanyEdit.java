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

import javax.faces.bean.ManagedBean;
import javax.faces.bean.ManagedProperty;
import javax.faces.context.FacesContext;
import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletRequest;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
public class CompanyEdit {

    private Company company = new Company();

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    public Company getCompany() {
        if (this.company == null) {
            String code = getCompanyCode();
            
            if (code != null && !code.isEmpty()) {
                EntityManager em = getContext().createEntityManager();
                
                try {
                    this.company = em.find(Company.class, code);
                }finally {
                    em.close();
                }
            }
        }

        return this.company;
    }

    public String save() {
        EntityManager em = getContext().createEntityManager();

        try {
            // Action Method
            return null;
        } finally {
            em.close();
        }
    }

    public EmployeeDemoContext getContext() {
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

    /**
     * Retrieve the 'companyCode' parameter from the {@link FacesContext}
     */
    protected String getCompanyCode() {
        FacesContext context = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) context.getExternalContext().getRequest();
        return myRequest.getParameter("companyCode");
    }

}
