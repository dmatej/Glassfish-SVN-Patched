/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package model;

import java.io.Serializable;
import java.sql.Time;
import java.util.ArrayList;
import java.util.List;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.2
 */
public class Employee implements Serializable {
    private int id;
    private String firstName;
    private String lastName;
    private Gender gender;
    private Time startTime;
    private Time endTime;
    private double salary;
    private long version;
    private EmploymentPeriod period = new EmploymentPeriod();
    private Address address;
    private List<Project> projects;
    private Employee manager;
    private List<Employee> managedEmployees;
    private List<PhoneNumber> phoneNumbers;
    private List<String> responsibilities;

    public Employee() {
        this.projects = new ArrayList<Project>();
        this.managedEmployees = new ArrayList<Employee>();
        this.phoneNumbers = new ArrayList<PhoneNumber>();
        this.responsibilities = new ArrayList<String>();
        this.period = new EmploymentPeriod();
    }
    
    public Employee(String firstName, String lastName) {
        this();
        this.firstName = firstName;
        this.lastName = lastName;
    }

    public int getId() {
        return id;
    }

    public void setId(int empId) {
        this.id = empId;
    }

    public Time getEndTime() {
        return this.endTime;
    }

    public void setEndTime(Time endTime) {
        this.endTime = endTime;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String fName) {
        this.firstName = fName;
    }

    public Gender getGender() {
        return this.gender;
    }

    public void setGender(Gender gender) {
        this.gender = gender;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lName) {
        this.lastName = lName;
    }

    public Time getStartTime() {
        return startTime;
    }

    public void setStartTime(Time startTime) {
        this.startTime = startTime;
    }

    public Long getVersion() {
        return version;
    }

    public List<Project> getProjects() {
        return projects;
    }

    public Project addProject(Project project) {
        getProjects().add(project);
        return project;
    }

    public Project removeProject(Project project) {
        List<Project> projects = getProjects();
        projects.remove(project);
        return project;
    }

    public Employee getManager() {
        return manager;
    }

    public void setManager(Employee employee) {
        this.manager = employee;
    }

    public List<Employee> getManagedEmployees() {
        return this.managedEmployees;
    }

    public void setManagedEmployees(List<Employee> employeeList) {
        this.managedEmployees = employeeList;
    }

    public Employee addManagedEmployee(Employee employee) {
        getManagedEmployees().add(employee);
        employee.setManager(this);
        return employee;
    }

    public Employee removeManagedEmployee(Employee employee) {
        getManagedEmployees().remove(employee);
        employee.setManager(null);
        return employee;
    }

    public List<PhoneNumber> getPhoneNumbers() {
        return phoneNumbers;
    }

    public void setPhoneNumbers(List<PhoneNumber> phoneNumberList) {
        this.phoneNumbers = phoneNumberList;
    }

    public PhoneNumber addPhoneNumber(PhoneNumber phoneNumber) {
        getPhoneNumbers().add(phoneNumber);
        phoneNumber.setOwner(this);
        return phoneNumber;
    }

    public PhoneNumber addPhoneNumber(String type, String areaCode, String number) {
        PhoneNumber phoneNumber = new PhoneNumber(type, areaCode, number);
        return addPhoneNumber(phoneNumber);
    }

    public PhoneNumber removePhoneNumber(PhoneNumber phoneNumber) {
        getPhoneNumbers().remove(phoneNumber);
        phoneNumber.setOwner(null);
        return phoneNumber;
    }

    public void setAddress(Address address) {
        this.address = address;
    }

    public Address getAddress() {
        return address;
    }

    public void setPeriod(EmploymentPeriod period) {
        this.period = period;
    }

    public void setPeriod() {
        setPeriod(new EmploymentPeriod());
    }

    public EmploymentPeriod getPeriod() {
        return period;
    }

    public double getSalary() {
        return salary;
    }

    public void setSalary(double salary) {
        this.salary = salary;
    }

    public List<String> getResponsibilities() {
        return this.responsibilities;
    }

    public void setResponsibilities(List<String> responsibilities) {
        this.responsibilities = responsibilities;
    }

    public void addResponsibility(String responsibility) {
        getResponsibilities().add(responsibility);
    }

    public void removeResponsibility(String responsibility) {
        getResponsibilities().remove(responsibility);
    }

    public String toString() {
        return "Employee(" + getId() + " - version=" + getVersion() + ")";
    }
}
