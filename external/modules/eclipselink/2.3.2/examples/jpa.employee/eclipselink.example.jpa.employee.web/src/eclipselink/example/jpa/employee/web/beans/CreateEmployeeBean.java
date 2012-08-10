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

import javax.faces.context.FacesContext;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.PhoneNumber;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class CreateEmployeeBean extends BaseManagedBean {

    protected static final String NAV = "edit";

    private Employee employee;
    private String newPhoneType;
    private String newResponsibility;

    private EmployeeService service;

    protected EmployeeService getService() {
        return this.service;
    }

    public Employee getEmployee() {
        return employee;
    }

    public String getNewPhoneType() {
        return newPhoneType;
    }

    public void setNewPhoneType(String newPhoneType) {
        this.newPhoneType = newPhoneType;
    }

    public String getNewResponsibility() {
        return newResponsibility;
    }

    public void setNewResponsibility(String newResponsibility) {
        this.newResponsibility = newResponsibility;
    }

    public void setEmployee(Employee employee) {
        this.employee = employee;
    }

    public String addAddress() {
        if (getEmployee().getAddress() == null) {
            getEmployee().setAddress(new Address());
        }
        return NAV;
    }

    public String removeAddress() {
        getEmployee().setAddress(null);
        return NAV;
    }

    public String addPhone() {
        if (getNewPhoneType() != null && !getNewPhoneType().isEmpty()) {
            getEmployee().addPhoneNumber(getNewPhoneType(), "", "");
            this.newPhoneType = null;
        }
        return NAV;
    }

    public String removePhone() {
        String type = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("phone-type");

        if (type != null && !type.isEmpty()) {
            PhoneNumber phone = getEmployee().getPhoneNumber(type);

            if (phone != null) {
                getEmployee().removePhoneNumber(phone);
            }
        }
        return NAV;
    }

    public String create() {
        getService().createEmployee(getCurrentUser(), getEmployee());
        this.employee = null;
        // TODO: pass emp-id to view
        return ViewEmployeeBean.NAV;
    }
}
