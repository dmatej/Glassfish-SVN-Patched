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

import javax.ejb.EJB;
import javax.faces.context.FacesContext;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.PhoneNumber;

public class EditEmployeeBean extends BaseManagedBean {

    protected static final String EMP_ID_PARAM = "selected-emp-id";

    private Employee employee;

    private String newPhoneType;

    private String newResponsibility;
    
    private String errorMessage = null;

    @EJB
    private EmployeeService service;

    protected EmployeeService getService() {
        return this.service;
    }

    public Employee getEmployee() {
        if (this.employee == null) {
            this.employee = getSelectedEmployee();
        }
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

    public String getErrorMessage() {
        return errorMessage;
    }

    public Employee getSelectedEmployee() {
        String empId = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get(EMP_ID_PARAM);

        if (empId != null && !empId.isEmpty()) {
            Employee emp = getService().findById(getCurrentUser(), Integer.valueOf(empId));

            // TODO: Replace with LoadGroup in EclipseLink 2.1
            if (emp != null) {
                emp.getAddress();
                emp.getPhoneNumbers().size();
                emp.getManager();
                emp.getManagedEmployees().size();
                emp.getProjects().size();
            }
            return emp;
        }

        return null;
    }

    public String addAddress() {
        if (getEmployee().getAddress() == null) {
            getEmployee().setAddress(new Address());
        }
        return "edit";
    }

    public String removeAddress() {
        getEmployee().setAddress(null);
        return "edit";
    }

    public String addPhone() {
        if (getNewPhoneType() != null && !getNewPhoneType().isEmpty()) {
            getEmployee().addPhoneNumber(getNewPhoneType(), "", "");
            this.newPhoneType = null;
        }
        return "edit";
    }

    public String removePhone() {
        String type = FacesContext.getCurrentInstance().getExternalContext().getRequestParameterMap().get("phone-type");

        if (type != null && !type.isEmpty()) {
            PhoneNumber phone = getEmployee().getPhoneNumber(type);

            if (phone != null) {
                getEmployee().removePhoneNumber(phone);
            }
        }
        return "edit";
    }

    public String save() {
        Employee result = getService().editEmployee(getCurrentUser(), getEmployee());
        
        if (result != null) {
            this.employee = result;
            return "success";
        }
        
        // Failure occurred during write
        this.errorMessage = "Save failed. Refresh may be required.";
        return null;
    }

    public String refresh() {
        this.employee = getService().refresh(getCurrentUser(), getEmployee());
        return "edit";
    }

    public String view() {
        this.employee = null;
        return ViewEmployeeBean.NAV;
    }
}
