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
import javax.persistence.EntityManager;
import javax.persistence.TypedQuery;

import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
public class CompanySearch {

    @ManagedProperty(value="#{employeeDemoContext}")
	private EmployeeDemoContext context;

	private String pattern = "%";
	
	private List<Company> companies;

	public EmployeeDemoContext getContext() {
		return context;
	}

	public void setContext(EmployeeDemoContext context) {
		this.context = context;
	}

	public String getPattern() {
		return pattern;
	}

	public void setPattern(String pattern) {
		this.pattern = pattern;
	}
	
	public List<Company> getCompanies() {
		if (this.companies == null) {
			setPattern("%");
			search();
		}
		return companies;
	}

	public String search() {
        EntityManager em = getContext().createEntityManager();
		
		try {
			TypedQuery<Company> query = em.createQuery("SELECT c FROM Company c WHERE c.name LIKE :PATTERN", Company.class);
			query.setParameter("PATTERN", getPattern());
			this.companies = query.getResultList();
		return null;
		} finally {
			em.close();
		}
	}
	
	public String delete(String id) {
		return null;
	}
}
