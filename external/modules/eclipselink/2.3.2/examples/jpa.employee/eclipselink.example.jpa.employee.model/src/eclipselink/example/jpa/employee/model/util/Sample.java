/*******************************************************************************
 * Copyright (c) 2007, 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - JPA Employee example using XML (bug 217884)
 *      dclarke - 2.1.2 - Enhanced for Company and Temporal member filtering
 ******************************************************************************/
package eclipselink.example.jpa.employee.model.util;

import java.sql.Date;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import javax.persistence.EntityManager;

import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.ReportQuery;
import org.eclipse.persistence.sessions.UnitOfWork;

import eclipselink.example.jpa.employee.model.Address;
import eclipselink.example.jpa.employee.model.Company;
import eclipselink.example.jpa.employee.model.Employee;
import eclipselink.example.jpa.employee.model.EmploymentPeriod;
import eclipselink.example.jpa.employee.model.Gender;
import eclipselink.example.jpa.employee.model.LargeProject;
import eclipselink.example.jpa.employee.model.Project;
import eclipselink.example.jpa.employee.model.SmallProject;

/**
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class Sample {

    private Company company;
    public Employee[] employees = null;
    private Project[] smallProjects = null;
    private LargeProject[] largeProjects = null;
    private long asOf;

    public Sample(Company company) {
        this.asOf = System.currentTimeMillis();
        
        this.company = company;
        this.employees = new Employee[] { basicEmployeeExample1(), 
                                          basicEmployeeExample2(), 
                                          basicEmployeeExample3(), 
                                          basicEmployeeExample4(), 
                                          basicEmployeeExample5(), 
                                          basicEmployeeExample6(), 
                                          basicEmployeeExample7(), 
                                          basicEmployeeExample8(), 
                                          basicEmployeeExample9(), 
                                          basicEmployeeExample10(), 
                                          basicEmployeeExample11(), 
                                          basicEmployeeExample12() };
    
        this.smallProjects = new Project[] { basicSmallProjectExample1(), 
                                             basicSmallProjectExample2(), 
                                             basicSmallProjectExample3(), 
                                             basicSmallProjectExample4(), 
                                             basicSmallProjectExample5(), 
                                             basicSmallProjectExample7(), 
                                             basicSmallProjectExample8(), 
                                             basicSmallProjectExample9(), 
                                             basicSmallProjectExample10() };
        
        this.largeProjects = new LargeProject[] { basicLargeProjectExample1(), 
                                                  basicLargeProjectExample2(), 
                                                  basicLargeProjectExample3(), 
                                                  basicLargeProjectExample4(), 
                                                  basicLargeProjectExample5() };
        
        // Setup management hierarchy
        addManagedEmployees(0, new int[] { 2, 3, 4 });
        addManagedEmployees(1, new int[] { 5, 0 });
        addManagedEmployees(2, new int[] {});
        addManagedEmployees(3, new int[] {});
        addManagedEmployees(4, new int[] {});
        addManagedEmployees(5, new int[] {});
        addManagedEmployees(6, new int[] {});
        addManagedEmployees(7, new int[] {});
        addManagedEmployees(8, new int[] {});
        addManagedEmployees(9, new int[] { 7, 8, 10, 11 });
        addManagedEmployees(10, new int[] { 6 });
        addManagedEmployees(11, new int[] { 1 });

        // Setup Employee-Project associations
        addProjects(0, new int[] { 0, 1, 2 }, new int[] {});
        addProjects(1, new int[] { 3, 4, 0 }, new int[] {});
        addProjects(2, new int[] { 3 }, new int[] { 3, 4 });
        addProjects(4, new int[] { 3, 1 }, new int[] { 2, 4 });
        addProjects(5, new int[] {}, new int[] { 1 });
        addProjects(6, new int[] {}, new int[] { 1 });

        // TODO-dclarke: Setup LargeProject leads
        // this.largeProjects[0].setTeamLeader(this.employees[1]);
        // this.largeProjects[3].setTeamLeader(this.employees[2]);
        // this.largeProjects[4].setTeamLeader(this.employees[2]);
    }
    
    public long getAsOf() {
        return this.asOf;
    }
    
    public long getAsOf(int adjustYear, int adjustMonth) {
        Calendar cal = Calendar.getInstance();
        cal.setTimeInMillis(getAsOf());
        
        int newMonth = cal.get(Calendar.MONTH) + adjustMonth;
        if (newMonth < 0) {
            adjustYear--;
            adjustMonth += 12;
        } else if (newMonth > 11) {
            adjustYear++;
            adjustMonth -= 12;
        }
        
        // adjust point in time
        cal.set(Calendar.YEAR, cal.get(Calendar.YEAR) + adjustYear);
        cal.set(Calendar.MONTH, cal.get(Calendar.MONTH) + adjustMonth);
        
        return cal.getTimeInMillis();
    }
    
    public Company getCompany() {
        return company;
    }

    public Employee basicEmployeeExample1() {
        Employee employee = new Employee();

        employee.setFirstName("Bob");
        employee.setLastName("Smith");
        employee.setGender(Gender.Male);
        employee.setSalary(35000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-10, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Toronto");
        address.setPostalCode("L5J2B5");
        address.setProvince("ONT");
        address.setStreet("1450 Acme Cr., Suite 4");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Work", "613", "5558812");

        return employee;
    }

    public Employee basicEmployeeExample10() {
        Employee employee = new Employee();

        employee.setFirstName("Jill");
        employee.setLastName("May");
        employee.setGender(Gender.Female);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-12, 2)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Calgary");
        address.setPostalCode("J5J2B5");
        address.setProvince("AB");
        address.setStreet("1111 Mooseland Rd.");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.setSalary(56232);
        employee.addPhoneNumber("Work", "613", "5558812");
        employee.addPhoneNumber("Work Fax", "613", "5555943");

        return employee;
    }

    public Employee basicEmployeeExample11() {
        Employee employee = new Employee();

        employee.setFirstName("Sarah-Lou");
        employee.setLastName("Smitty");
        employee.setGender(Gender.Female);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-4, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Arnprior");
        address.setPostalCode("W1A2B5");
        address.setProvince("ONT");
        address.setStreet("1 Hawthorne Drive");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.setSalary(75000);
        employee.addPhoneNumber("Work Fax", "613", "5555943");
        employee.addPhoneNumber("Home", "613", "5551234");
        employee.addPhoneNumber("Cellular", "416", "5551111");

        return employee;
    }

    public Employee basicEmployeeExample12() {
        Employee employee = new Employee();

        employee.setFirstName("Jim-Bob");
        employee.setLastName("Jefferson");
        employee.setGender(Gender.Male);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-3, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Yellowknife");
        address.setPostalCode("Y5J2N5");
        address.setProvince("YK");
        address.setStreet("1112 Gold Rush Rd.");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.setSalary(50000);
        employee.addPhoneNumber("Home", "613", "5551234");
        employee.addPhoneNumber("Cellular", "416", "5551111");

        return employee;
    }

    public Employee basicEmployeeExample2() {
        Employee employee = new Employee();

        employee.setFirstName("John");
        employee.setLastName("Way");
        employee.setGender(Gender.Male);
        employee.setSalary(53000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-3, -1)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Ottawa");
        address.setPostalCode("K5J2B5");
        address.setProvince("ONT");
        address.setStreet("12 Merivale Rd., Suite 5");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Work", "613", "5558812");
        employee.addPhoneNumber("ISDN", "905", "5553691");

        return employee;
    }

    public Employee basicEmployeeExample3() {
        Employee employee = new Employee();

        employee.setFirstName("Charles");
        employee.setLastName("Chanley");
        employee.setGender(Gender.Male);
        employee.setSalary(43000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-7, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Montreal");
        address.setPostalCode("Q2S5Z5");
        address.setProvince("QUE");
        address.setStreet("1 Canadien Place");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Pager", "976", "5556666");
        employee.addPhoneNumber("ISDN", "905", "5553691");

        return employee;
    }

    public Employee basicEmployeeExample4() {
        Employee employee = new Employee();

        employee.setFirstName("Emanual");
        employee.setLastName("Smith");
        employee.setGender(Gender.Male);
        employee.setSalary(49631);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-1, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Vancouver");
        address.setPostalCode("N5J2N5");
        address.setProvince("BC");
        address.setStreet("20 Mountain Blvd., Floor 53, Suite 6");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Work Fax", "613", "5555943");
        employee.addPhoneNumber("Cellular", "416", "5551111");
        employee.addPhoneNumber("Pager", "976", "5556666");
        employee.addPhoneNumber("ISDN", "905", "5553691");

        return employee;
    }

    public Employee basicEmployeeExample5() {
        Employee employee = new Employee();

        employee.setFirstName("Sarah");
        employee.setLastName("Way");
        employee.setGender(Gender.Female);
        employee.setSalary(87000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(0, -8)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Prince Rupert");
        address.setPostalCode("K3K5D5");
        address.setProvince("BC");
        address.setStreet("3254 Parkway Place");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Work", "613", "5558812");
        employee.addPhoneNumber("ISDN", "905", "5553691");
        employee.addPhoneNumber("Home", "613", "5551234");

        return employee;
    }

    public Employee basicEmployeeExample6() {
        Employee employee = new Employee();

        employee.setFirstName("Marcus");
        employee.setLastName("Saunders");
        employee.setGender(Gender.Male);
        employee.setSalary(54300);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-3, -6)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Perth");
        address.setPostalCode("Y3Q2N9");
        address.setProvince("ONT");
        address.setStreet("234 Caledonia Lane");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("ISDN", "905", "5553691");
        employee.addPhoneNumber("Work", "613", "5558812");

        return employee;
    }

    public Employee basicEmployeeExample7() {
        Employee employee = new Employee();

        employee.setFirstName("Nancy");
        employee.setLastName("White");
        employee.setGender(Gender.Female);
        employee.setSalary(31000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-8, 0)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Metcalfe");
        address.setPostalCode("Y4F7V6");
        address.setProvince("ONT");
        address.setStreet("2 Anderson Rd.");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Home", "613", "5551234");

        return employee;
    }

    public Employee basicEmployeeExample8() {
        Employee employee = new Employee();

        employee.setFirstName("Fred");
        employee.setLastName("Jones");
        employee.setGender(Gender.Male);
        employee.setSalary(500000);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-3, 1)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Victoria");
        address.setPostalCode("Z5J2N5");
        address.setProvince("BC");
        address.setStreet("382 Hyde Park Blvd.");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Cellular", "416", "5551111");
        employee.addPhoneNumber("ISDN", "905", "5553691");

        return employee;
    }

    public Employee basicEmployeeExample9() {
        Employee employee = new Employee();

        employee.setFirstName("Betty");
        employee.setLastName("Jones");
        employee.setGender(Gender.Female);
        employee.setSalary(500001);

        EmploymentPeriod employmentPeriod = new EmploymentPeriod();
        employmentPeriod.setStartDate(new Date(getAsOf(-3, 11)));
        employee.setPeriod(employmentPeriod);

        Address address = new Address();
        address.setCity("Smith Falls");
        address.setPostalCode("C6C6C6");
        address.setProvince("ONT");
        address.setStreet("89 Chocolate Drive");
        address.setCountry("Canada");
        employee.setAddress(address);

        employee.addPhoneNumber("Work", "613", "5558812");
        employee.addPhoneNumber("ISDN", "905", "5553691");

        return employee;
    }

    public LargeProject basicLargeProjectExample1() {
        LargeProject largeProject = new LargeProject();

        largeProject.setName("Sales Reporting");
        largeProject.setDescription("A reporting application to report on the corporations database through TopLink.");
        largeProject.setBudget((double) 5000);
        largeProject.getMilestone().set(1991, 10, 11, 12, 0, 0);

        return largeProject;
    }

    public LargeProject basicLargeProjectExample2() {
        LargeProject largeProject = new LargeProject();

        largeProject.setName("Light Reporter");
        largeProject.setDescription("A lightweight application to report on the corporations database through TopLink.");
        largeProject.setBudget(100.98);
        largeProject.getMilestone().set(1999, 11, 25, 11, 40, 44);

        return largeProject;
    }

    public LargeProject basicLargeProjectExample3() {
        LargeProject largeProject = new LargeProject();

        largeProject.setName("TOPEmployee Management");
        largeProject.setDescription("A management application to report on the corporations database through TopLink.");
        largeProject.setBudget(4000.98);
        largeProject.getMilestone().set(1997, 10, 12, 1, 0, 0);

        return largeProject;
    }

    public LargeProject basicLargeProjectExample4() {
        LargeProject largeProject = new LargeProject();

        largeProject.setName("Enterprise System");
        largeProject.setDescription("A enterprise wide application to report on the corporations database through TopLink.");
        largeProject.setBudget(40.98);
        largeProject.getMilestone().set(1996, 8, 6, 6, 40, 44);

        return largeProject;
    }

    public LargeProject basicLargeProjectExample5() {
        LargeProject largeProject = new LargeProject();

        largeProject.setName("Problem Reporting System");
        largeProject.setDescription("A PRS application to report on the corporations database through TopLink.");
        largeProject.setBudget(101.98);
        largeProject.getMilestone().set(1997, 9, 6, 1, 40, 44);

        return largeProject;
    }

    public SmallProject basicSmallProjectExample1() {
        SmallProject project =  new SmallProject("Enterprise");
        project.setDescription("An enterprise wide application to report on the corporations database through TopLink.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample10() {
        SmallProject project = new SmallProject("Staff Query Tool");
        project.setDescription("A tool to help staff query various things.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample2() {
        SmallProject project = new SmallProject("Sales Reporter");
        project.setDescription("A reporting application using JDK to report on the corporations database through TopLink.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample3() {
        SmallProject project = new SmallProject("Employee Manager");
        project.setDescription("A management application to report on the corporations database through TopLink.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample4() {
        SmallProject project = new SmallProject("Problem Reporter");
        project.setDescription("A PRS application to report on the corporations database through TopLink.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample5() {
        SmallProject project = new SmallProject("Feather Reporter");
        project.setDescription("An extremely lightweight application to report on the corporations database through TopLink.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample6() {
        SmallProject project = new SmallProject("Makework");
        project.setDescription("A makework project.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample7() {
        SmallProject project = new SmallProject("Marketing Query Tool");
        project.setDescription("A tool to help marketing query various things.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample8() {
        SmallProject project =  new SmallProject("Shipping Query Tool");
        project.setDescription("A tool to help shipping query various things.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    public SmallProject basicSmallProjectExample9() {
        SmallProject project = new SmallProject("Accounting Query Tool");
        project.setDescription("A tool to help accounting query various things.");
        project.setStartDate(null);
        project.setEndDate(null);
        return project;
    }

    private void addManagedEmployees(int managerIndex, int[] employeeIndeces) {
        Employee manager = this.employees[managerIndex];

        if (manager.getManagedEmployees().isEmpty()) {
            for (int index = 0; index < employeeIndeces.length; index++) {
                manager.addManagedEmployee(this.employees[employeeIndeces[index]]);
            }
        }
    }

    private void addProjects(int empIndex, int[] smallProjIndeces, int[] largeProjIndeces) {
        Employee employee = this.employees[empIndex];

        for (int index = 0; index < smallProjIndeces.length; index++) {
            employee.addProject(this.smallProjects[smallProjIndeces[index]], null);
        }

        for (int index = 0; index < largeProjIndeces.length; index++) {
            employee.addProject(this.largeProjects[largeProjIndeces[index]], null);
        }
    }

    /**
     * Register all of the population in the provided EntityManager to be
     * persisted This method should only be called from within a test case. It
     * asserts that the provided EntityManager is in a transaction and that the
     * database tables are empty.
     */
    public void persistAll(EntityManager em, boolean verify) {
        // if (!em.getTransaction().isActive()) {
        // throw new IllegalStateException("EntityManager no in transaction");
        // }

        // Verify that the database tables are empty
        if (verify) {
            verifyCount(em, Employee.class, 0);
            verifyCount(em, Address.class, 0);
            verifyCount(em, Project.class, 0);
        }

        for (int index = 0; index < this.employees.length; index++) {
            em.persist(this.employees[index]);
        }
        for (int index = 0; index < this.smallProjects.length; index++) {
            em.persist(this.smallProjects[index]);
        }
        for (int index = 0; index < this.largeProjects.length; index++) {
            em.persist(this.largeProjects[index]);
        }

        em.flush();

        if (verify) {
            verifyCounts(em);
        }
    }

    public void verifyCounts(EntityManager em) {
        verifyCount(em, Employee.class, this.employees.length);
        verifyCount(em, Address.class, this.employees.length);
        verifyCount(em, Project.class, this.smallProjects.length + this.largeProjects.length);
    }

    /**
     * Verify that the provided entity type has no rows in the database using a
     * native ReportQuery.
     * 
     * @param entityClass
     * @param count
     */
    public void verifyCount(EntityManager em, Class<?> entityClass, int count) {
        UnitOfWork uow = JpaHelper.getEntityManager(em).getUnitOfWork();

        ExpressionBuilder eb = new ExpressionBuilder();
        ReportQuery query = new ReportQuery(entityClass, eb);
        // TODO: Add additional criteria?
        query.addCount();
        query.setShouldReturnSingleValue(true);

        // Need to set the descriptor or the default query redirector will not
        // be used
        query.setDescriptor(uow.getClassDescriptor(entityClass));

        int dbCount = ((Number) uow.executeQuery(query)).intValue();

        if (count != dbCount) {
            throw new RuntimeException("Incorrect quantity found of " + entityClass + ": expected: " + count + " found: " + dbCount);
        }
    }

    /**
     * Verify that the provided list of Employee instances matches the sample
     * population.
     * 
     * @param employees
     */
    public void verifySame(List<Employee> dbEmps) {
        if (this.employees.length != dbEmps.size()) {
            throw new RuntimeException("Incorrect quantity of employees. Expected: " + this.employees.length + " found: " + dbEmps.size());
        }

        Collections.sort(dbEmps, new EmployeeComparator());

        for (int index = 0; index < this.employees.length; index++) {
            Employee emp = employees[index];
            Employee dbEmp = dbEmps.get(index);

            if (!emp.getFirstName().equals(dbEmp.getFirstName())) {
                throw new RuntimeException("First name does not match on employees[" + index + "] - expected:" + emp.getFirstName() + " found: " + dbEmp.getFirstName());
            }
            if (!emp.getLastName().equals(dbEmp.getLastName())) {
                throw new RuntimeException("Last name does not match on employees[" + index + "] - expected:" + emp.getLastName() + " found: " + dbEmp.getLastName());
            }
            if (emp.getSalary() != dbEmp.getSalary()) {
                throw new RuntimeException("Salary does not match on employees[" + index + "] - expected:" + emp.getSalary() + " found: " + dbEmp.getSalary());
            }
        }
    }

    /**
     * Simple comparator used to order the employees for use within assertSame
     */
    class EmployeeComparator implements Comparator<Employee> {

        public int compare(Employee emp1, Employee emp2) {
            return emp1.getId() - emp2.getId();
        }

    }

}
