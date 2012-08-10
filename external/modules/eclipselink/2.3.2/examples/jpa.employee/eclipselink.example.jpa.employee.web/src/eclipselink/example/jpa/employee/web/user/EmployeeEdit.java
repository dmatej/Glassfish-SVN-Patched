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
import javax.faces.bean.ManagedProperty;
import javax.faces.bean.SessionScoped;
import javax.faces.context.FacesContext;
import javax.persistence.EntityManager;
import javax.servlet.http.HttpServletRequest;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.PhoneNumber;
import eclipselink.example.jpa.employee.web.EmployeeDemoContext;

@ManagedBean
@SessionScoped
public class EmployeeEdit {

    private String errorMessage;

    private String newPhoneType;

    private Employee employee;

    private EntityManager entityManager;

    @ManagedProperty(value = "#{employeeDemoContext}")
    private EmployeeDemoContext context;

    public EmployeeDemoContext getContext() {
        if (this.context == null) {
        }
        return context;
    }

    public void setContext(EmployeeDemoContext context) {
        this.context = context;
    }

    public EntityManager getEntityManager() {
        if (this.entityManager == null) {
            this.entityManager = getContext().createEntityManager();

            if (getEmployee() != null) {
                this.employee = this.entityManager.find(Employee.class, employee.getId());
            }
        }
        return entityManager;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public String getNewPhoneType() {
        return newPhoneType;
    }

    public void setNewPhoneType(String newPhoneType) {
        this.newPhoneType = newPhoneType;
    }

    public Employee getEmployee() {
        if (this.employee == null) {
            Integer empId = getEmployeeId();

            if (empId != null) {
                this.employee = getEntityManager().find(Employee.class, empId);
            }
        }
        return employee;
    }

    /**
     * Retrieve the 'employeeId' parameter from the {@link FacesContext}
     * 
     * @return
     */
    protected Integer getEmployeeId() {
        FacesContext context = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) context.getExternalContext().getRequest();
        Object empId = myRequest.getParameter("employeeId");
        return Integer.valueOf((String) empId);
    }

    public String save() {
        EntityManager em = getEntityManager();

        em.getTransaction().begin();
        em.getTransaction().commit();

        return cancel();
    }

    /**
     * Cancel the pending edit of the employee and return to the view
     */
    public String cancel() {
        if (this.entityManager != null) {
            getEntityManager().clear();
            this.entityManager = null;
            this.employee = null;
        }

        return "employee_view";
    }

    public String addAddress() {
        getEmployee().setAddress(new Address());
        return "employee_edit";
    }

    public String removeAddress() {
        getEmployee().setAddress(null);
        return "employee_edit";
    }

    public String addPhone() {
        getEmployee().addPhoneNumber(getNewPhoneType(), "", "");
        return "employee_edit";
    }

    public String removePhone() {
        FacesContext context = FacesContext.getCurrentInstance();
        HttpServletRequest myRequest = (HttpServletRequest) context.getExternalContext().getRequest();
        String type = myRequest.getParameter("phone-type");

        PhoneNumber phone = getEmployee().getPhoneNumber(type);

        if (phone != null) {
            employee.removePhoneNumber(phone);
            getEntityManager().remove(phone);
        }

        return "employee_edit";
    }
}
