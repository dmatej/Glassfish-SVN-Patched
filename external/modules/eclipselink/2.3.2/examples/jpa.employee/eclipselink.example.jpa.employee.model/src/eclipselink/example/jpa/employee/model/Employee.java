/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              dclarke - initial JPA Employee example using XML (bug 217884)
 *              mbraeuer - annotated version, transformation
 ******************************************************************************/
package eclipselink.example.jpa.employee.model;

import static javax.persistence.CascadeType.ALL;
import static javax.persistence.FetchType.LAZY;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Basic;
import javax.persistence.Embedded;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Transient;
import javax.persistence.Version;

import org.eclipse.persistence.annotations.Multitenant;
import org.eclipse.persistence.annotations.MultitenantType;
import org.eclipse.persistence.annotations.TenantDiscriminatorColumn;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
@Entity
@Multitenant(MultitenantType.SINGLE_TABLE)
@TenantDiscriminatorColumn(name="CC")
public class Employee {
    
    @Id
    private int id;

    private String firstName;

    /**
     * Gender mapped using Basic with an ObjectTypeConverter to map between
     * single char code value in database to enum. JPA only supports mapping to
     * the full name of the enum or its ordinal value.
     */
    @Basic
    private Gender gender = Gender.Male;

    private String lastName;

    private double salary;

    @Version
    private Long version;

    @Transient
    //@OneToMany(mappedBy = "employee", cascade = CascadeType.ALL)
    private List<Member> projectMemberships = new ArrayList<Member>();

    @ManyToOne(fetch = LAZY)
    private Employee manager;

    @OneToMany(mappedBy = "manager")
    private List<Employee> managedEmployees = new ArrayList<Employee>();

    @OneToMany(mappedBy = "owner", cascade = ALL)
    private List<PhoneNumber> phoneNumbers = new ArrayList<PhoneNumber>();

    @OneToOne(cascade = ALL, fetch = LAZY)
    private Address address;

    @Embedded
    private EmploymentPeriod period;

    @Transient 
    private Map<String, Object> attributes = new HashMap<String, Object>();

    public Employee() {
    }

    public int getId() {
        return id;
    }

    public void setId(int empId) {
        this.id = empId;
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

    public void setGender(String gender) {
        this.gender = Gender.valueOf(gender);
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lName) {
        this.lastName = lName;
    }

    public String getDisplayName() {
        return getLastName() + ", " + getFirstName();
    }
    
    public Long getVersion() {
        return version;
    }

    public void setVersion(Long version) {
        this.version = version;
    }

    public List<Member> getProjects() {
        return this.projectMemberships;
    }

    public Member addProject(Project project, Timestamp start) {
        Member member = new Member(this, project, start, false);
        getProjects().add(member);
        return member;
    }

    public Project removeProject(Project project) {
        getProjects().remove(project);
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
        for (PhoneNumber phone : getPhoneNumbers()) {
            if (phone.getType().equalsIgnoreCase(phoneNumber.getType())) {
                throw new IllegalArgumentException("Duplicate phone number type: " + phoneNumber);
            }
        }
        getPhoneNumbers().add(phoneNumber);
        return phoneNumber;
    }

    public PhoneNumber addPhoneNumber(String type, String areaCode, String number) {
        PhoneNumber phoneNumber = new PhoneNumber(this, type, areaCode, number);
        return addPhoneNumber(phoneNumber);
    }

    public PhoneNumber removePhoneNumber(PhoneNumber phoneNumber) {
        getPhoneNumbers().remove(phoneNumber);
        //phoneNumber.setOwner(null);
        return phoneNumber;
    }

    public PhoneNumber getPhoneNumber(String type) {
        for (PhoneNumber phone : getPhoneNumbers()) {
            if (phone.getType().equalsIgnoreCase(type)) {
                return phone;
            }
        }
        return null;
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

    public EmploymentPeriod getPeriod() {
        return period;
    }

    public double getSalary() {
        return salary;
    }

    public void setSalary(double salary) {
        this.salary = salary;
    }

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public Object get(String name) {
        return getAttributes().get(name);
    }

    public Object set(String name, Object value) {
        return getAttributes().put(name, value);
    }

    public Object removeAttribute(String name) {
        return getAttributes().remove(name);
    }

    public String getIdString() {
        return this.id <= 0 ? "" : String.valueOf(this.id);
    }

    public String toString() {
        return "Employee(" + getId() + ": " + getLastName() + ", " + getFirstName() + ")";
    }
}
