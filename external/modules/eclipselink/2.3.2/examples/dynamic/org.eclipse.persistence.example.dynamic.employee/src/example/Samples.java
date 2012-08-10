/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Dynamic Persistence INCUBATION - Enhancement 200045
 *               http://wiki.eclipse.org/EclipseLink/Development/JPA/Dynamic
 *     
 * This code is being developed under INCUBATION and is not currently included 
 * in the automated EclipseLink build. The API in this code may change, or 
 * may never be included in the product. Please provide feedback through mailing 
 * lists or the bug database.
 ******************************************************************************/
package example;

/*******************************************************************************
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - Dynamic Persistence INCUBATION - Enhancement 200045
 *               http://wiki.eclipse.org/EclipseLink/Development/JPA/Dynamic
 *     
 * This code is being developed under INCUBATION and is not currently included 
 * in the automated EclipseLink build. The API in this code may change, or 
 * may never be included in the product. Please provide feedback through mailing 
 * lists or the bug database.
 ******************************************************************************/
import java.util.*;

import javax.persistence.*;

import junit.framework.Assert;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.dynamic.DynamicEntity;
import org.eclipse.persistence.expressions.ExpressionBuilder;
import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.queries.DeleteAllQuery;
import org.eclipse.persistence.queries.ReportQuery;

/**
 * 
 * @author dclarke
 * @since EclipseLink - Dynamic Incubator (1.1.0-branch)
 */
@SuppressWarnings("deprecation")
public class Samples {
    private EntityManagerFactory emf;
    public DynamicEntity[] employees;
    public DynamicEntity[] smallProjects;
    public DynamicEntity[] largeProjects;

    public Samples(EntityManagerFactory emf) {
        this.emf = emf;

        this.employees = new DynamicEntity[] { basicEmployeeExample1(), basicEmployeeExample2(), basicEmployeeExample3(), basicEmployeeExample4(), basicEmployeeExample5(), basicEmployeeExample6(), basicEmployeeExample7(), basicEmployeeExample8(), basicEmployeeExample9(), basicEmployeeExample10(), basicEmployeeExample11(), basicEmployeeExample12() };

        this.smallProjects = new DynamicEntity[] { basicSmallProjectExample1(), basicSmallProjectExample2(), basicSmallProjectExample3(), basicSmallProjectExample4(), basicSmallProjectExample5(), basicSmallProjectExample7(), basicSmallProjectExample8(), basicSmallProjectExample9(), basicSmallProjectExample10() };

        this.largeProjects = new DynamicEntity[] { basicLargeProjectExample1(), basicLargeProjectExample2(), basicLargeProjectExample3(), basicLargeProjectExample4(), basicLargeProjectExample5() };

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

        // Setup LargeProject leads
        this.largeProjects[0].set("teamLeader", this.employees[1]);
        this.largeProjects[3].set("teamLeader", this.employees[2]);
        this.largeProjects[4].set("teamLeader", this.employees[2]);

    }

    private DynamicEntity newInstance(String entityAlias) {
        ClassDescriptor descriptor = JpaHelper.getServerSession(this.emf).getDescriptorForAlias(entityAlias);
        return (DynamicEntity) descriptor.getInstantiationPolicy().buildNewInstance();
    }

    private Class<?> getDynamicClass(String entityAlias) {
        ClassDescriptor descriptor = JpaHelper.getServerSession(this.emf).getDescriptorForAlias(entityAlias);
        return descriptor.getJavaClass();
    }

    @SuppressWarnings("unchecked")
    private DynamicEntity addPhoneNumber(DynamicEntity employee, String type, String areaCode, String number) {
        DynamicEntity phone = newInstance("PhoneNumber");
        phone.set("type", type);
        phone.set("areaCode", areaCode);
        phone.set("number", number);
        phone.set("owner", employee);
        employee.<Collection> get("phoneNumbers").add(phone);
        return phone;
    }

    private void setPeriod(DynamicEntity employee, Date startDate, Date endDate) {
        DynamicEntity period = newInstance("EmploymentPeriod");
        period.set("startDate", startDate);
        period.set("endDate", endDate);
        employee.set("period", period);
    }

    public DynamicEntity basicEmployeeExample1() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Bob");
        employee.set("lastName", "Smith");
        employee.set("gender", "Male");
        employee.set("salary", 35000);

        setPeriod(employee, new Date(1993, 0, 1), new Date(1996, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Toronto");
        address.set("postalCode", "L5J2B5");
        address.set("province", "ONT");
        address.set("street", "1450 Acme Cr., Suite 4");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Water the office plants.");
        employee.<Collection<String>> get("responsibilities").add("Maintain the kitchen facilities.");
        addPhoneNumber(employee, "Work", "613", "5558812");

        return employee;
    }

    public DynamicEntity basicEmployeeExample10() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Jill");
        employee.set("lastName", "May");
        employee.set("gender", "Female");

        setPeriod(employee, new Date(1991, 10, 111), null);

        DynamicEntity address = newInstance("Address");
        address.set("city", "Calgary");
        address.set("postalCode", "J5J2B5");
        address.set("province", "AB");
        address.set("street", "1111 Mooseland Rd.");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.set("salary", 56232);
        addPhoneNumber(employee, "Work", "613", "5558812");
        addPhoneNumber(employee, "Work Fax", "613", "5555943");

        return employee;
    }

    public DynamicEntity basicEmployeeExample11() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Sarah-Lou");
        employee.set("lastName", "Smitty");
        employee.set("gender", "Female");

        setPeriod(employee, new Date(1993, 0, 1), new Date(1996, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Arnprior");
        address.set("postalCode", "W1A2B5");
        address.set("province", "ONT");
        address.set("street", "1 Hawthorne Drive");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.set("salary", 75000);
        addPhoneNumber(employee, "Work Fax", "613", "5555943");
        addPhoneNumber(employee, "Home", "613", "5551234");
        addPhoneNumber(employee, "Cellular", "416", "5551111");

        return employee;
    }

    public DynamicEntity basicEmployeeExample12() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Jim-Bob");
        employee.set("lastName", "Jefferson");
        employee.set("gender", "Male");

        setPeriod(employee, new Date(1995, 0, 12), new Date(2001, 11, 31));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Yellowknife");
        address.set("postalCode", "Y5J2N5");
        address.set("province", "YK");
        address.set("street", "1112 Gold Rush Rd.");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.set("salary", 50000);
        addPhoneNumber(employee, "Home", "613", "5551234");
        addPhoneNumber(employee, "Cellular", "416", "5551111");

        return employee;
    }

    public DynamicEntity basicEmployeeExample2() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "John");
        employee.set("lastName", "Way");
        employee.set("gender", "Male");
        employee.set("salary", 53000);

        setPeriod(employee, new Date(1991, 10, 11), null);

        DynamicEntity address = newInstance("Address");
        address.set("city", "Ottawa");
        address.set("postalCode", "K5J2B5");
        address.set("province", "ONT");
        address.set("street", "12 Merivale Rd., Suite 5");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Hire people when more people are required.");
        employee.<Collection<String>> get("responsibilities").add("Lay off employees when less people are required.");
        addPhoneNumber(employee, "Work", "613", "5558812");
        addPhoneNumber(employee, "ISDN", "905", "5553691");

        return employee;
    }

    public DynamicEntity basicEmployeeExample3() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Charles");
        employee.set("lastName", "Chanley");
        employee.set("gender", "Male");
        employee.set("salary", 43000);

        setPeriod(employee, new Date(1995, 0, 1), new Date(2001, 11, 31));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Montreal");
        address.set("postalCode", "Q2S5Z5");
        address.set("province", "QUE");
        address.set("street", "1 Canadien Place");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Perform code reviews as required.");

        addPhoneNumber(employee, "Pager", "976", "5556666");
        addPhoneNumber(employee, "ISDN", "905", "5553691");

        return employee;
    }

    public DynamicEntity basicEmployeeExample4() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Emanual");
        employee.set("lastName", "Smith");
        employee.set("gender", "Male");
        employee.set("salary", 49631);

        setPeriod(employee, new Date(2001, 11, 31), new Date(1995, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Vancouver");
        address.set("postalCode", "N5J2N5");
        address.set("province", "BC");
        address.set("street", "20 Mountain Blvd., Floor 53, Suite 6");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Have to fix the Database problem.");
        addPhoneNumber(employee, "Work Fax", "613", "5555943");
        addPhoneNumber(employee, "Cellular", "416", "5551111");
        addPhoneNumber(employee, "Pager", "976", "5556666");
        addPhoneNumber(employee, "ISDN", "905", "5553691");

        return employee;
    }

    public DynamicEntity basicEmployeeExample5() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Sarah");
        employee.set("lastName", "Way");
        employee.set("gender", "Female");
        employee.set("salary", 87000);

        setPeriod(employee, new Date(2001, 6, 31), new Date(1995, 4, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Prince Rupert");
        address.set("postalCode", "K3K5D5");
        address.set("province", "BC");
        address.set("street", "3254 Parkway Place");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Write code documentation.");
        addPhoneNumber(employee, "Work", "613", "5558812");
        addPhoneNumber(employee, "ISDN", "905", "5553691");
        addPhoneNumber(employee, "Home", "613", "5551234");

        return employee;
    }

    public DynamicEntity basicEmployeeExample6() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Marcus");
        employee.set("lastName", "Saunders");
        employee.set("gender", "Male");
        employee.set("salary", 54300);

        setPeriod(employee, new Date(2001, 11, 31), new Date(1995, 0, 12));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Perth");
        address.set("postalCode", "Y3Q2N9");
        address.set("province", "ONT");
        address.set("street", "234 Caledonia Lane");
        address.set("country", "Canada");
        employee.set("address", address);

        employee.<Collection<String>> get("responsibilities").add("Write user specifications.");
        addPhoneNumber(employee, "ISDN", "905", "5553691");
        addPhoneNumber(employee, "Work", "613", "5558812");

        return employee;
    }

    public DynamicEntity basicEmployeeExample7() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Nancy");
        employee.set("lastName", "White");
        employee.set("gender", "Female");
        employee.set("salary", 31000);

        setPeriod(employee, new Date(1996, 0, 1), new Date(1993, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Metcalfe");
        address.set("postalCode", "Y4F7V6");
        address.set("province", "ONT");
        address.set("street", "2 Anderson Rd.");
        address.set("country", "Canada");
        employee.set("address", address);

        addPhoneNumber(employee, "Home", "613", "5551234");

        return employee;
    }

    public DynamicEntity basicEmployeeExample8() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Fred");
        employee.set("lastName", "Jones");
        employee.set("gender", "Male");
        employee.set("salary", 500000);

        setPeriod(employee, new Date(2001, 11, 31), new Date(1995, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Victoria");
        address.set("postalCode", "Z5J2N5");
        address.set("province", "BC");
        address.set("street", "382 Hyde Park Blvd.");
        address.set("country", "Canada");
        employee.set("address", address);

        addPhoneNumber(employee, "Cellular", "416", "5551111");
        addPhoneNumber(employee, "ISDN", "905", "5553691");

        return employee;
    }

    public DynamicEntity basicEmployeeExample9() {
        DynamicEntity employee = newInstance("Employee");

        employee.set("firstName", "Betty");
        employee.set("lastName", "Jones");
        employee.set("gender", "Female");
        employee.set("salary", 500001);

        setPeriod(employee, new Date(2001, 11, 31), new Date(1995, 0, 1));

        DynamicEntity address = newInstance("Address");
        address.set("city", "Smith Falls");
        address.set("postalCode", "C6C6C6");
        address.set("province", "ONT");
        address.set("street", "89 Chocolate Drive");
        address.set("country", "Canada");
        employee.set("address", address);

        addPhoneNumber(employee, "Work", "613", "5558812");
        addPhoneNumber(employee, "ISDN", "905", "5553691");

        return employee;
    }

    private void setCalendar(DynamicEntity entity, String name, int year, int month, int day, int hour, int minute, int seconds) {
        Calendar cal = entity.<Calendar> get(name);

        if (cal == null) {
            cal = Calendar.getInstance();
            entity.set(name, cal);
        }
        cal.set(year, month, day, hour, minute, seconds);
    }

    public DynamicEntity basicLargeProjectExample1() {
        DynamicEntity largeProject = newInstance("LargeProject");

        largeProject.set("name", "Sales Reporting");
        largeProject.set("description", "A reporting application to report on the corporations database through TopLink.");
        largeProject.set("budget", (double) 5000);
        setCalendar(largeProject, "milestone", 1991, 10, 11, 12, 0, 0);

        return largeProject;
    }

    public DynamicEntity basicLargeProjectExample2() {
        DynamicEntity largeProject = newInstance("LargeProject");

        largeProject.set("name", "Light Reporter");
        largeProject.set("description", "A lightweight application to report on the corporations database through TopLink.");
        largeProject.set("budget", 100.98);
        setCalendar(largeProject, "milestone", 1999, 11, 25, 11, 40, 44);

        return largeProject;
    }

    public DynamicEntity basicLargeProjectExample3() {
        DynamicEntity largeProject = newInstance("LargeProject");

        largeProject.set("name", "TOPEmployee Management");
        largeProject.set("description", "A management application to report on the corporations database through TopLink.");
        largeProject.set("budget", 4000.98);
        setCalendar(largeProject, "milestone", 1997, 10, 12, 1, 0, 0);

        return largeProject;
    }

    public DynamicEntity basicLargeProjectExample4() {
        DynamicEntity largeProject = newInstance("LargeProject");

        largeProject.set("name", "Enterprise System");
        largeProject.set("description", "A enterprise wide application to report on the corporations database through TopLink.");
        largeProject.set("budget", 40.98);
        setCalendar(largeProject, "milestone", 1996, 8, 6, 6, 40, 44);

        return largeProject;
    }

    public DynamicEntity basicLargeProjectExample5() {
        DynamicEntity largeProject = newInstance("LargeProject");

        largeProject.set("name", "Problem Reporting System");
        largeProject.set("description", "A PRS application to report on the corporations database through TopLink.");
        largeProject.set("budget", 101.98);
        setCalendar(largeProject, "milestone", 1997, 9, 6, 1, 40, 44);

        return largeProject;
    }

    public DynamicEntity basicSmallProjectExample1() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Enterprise");
        smallProject.set("description", "A enterprise wide application to report on the corporations database through TopLink.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample10() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Staff Query Tool");
        smallProject.set("description", "A tool to help staff query various things.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample2() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Sales Reporter");
        smallProject.set("description", "A reporting application using JDK to report on the corporations database through TopLink.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample3() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "TOP-Employee Manager");
        smallProject.set("description", "A management application to report on the corporations database through TopLink.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample4() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Problem Reporter");
        smallProject.set("description", "A PRS application to report on the corporations database through TopLink.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample5() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Feather Reporter");
        smallProject.set("description", "An extremely lightweight application to report on the corporations database through TopLink.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample6() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Makework");
        smallProject.set("description", "A makework project.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample7() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Marketing Query Tool");
        smallProject.set("description", "A tool to help marketing query various things.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample8() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Shipping Query Tool");
        smallProject.set("description", "A tool to help shipping query various things.");
        return smallProject;
    }

    public DynamicEntity basicSmallProjectExample9() {
        DynamicEntity smallProject = newInstance("SmallProject");
        smallProject.set("name", "Accounting Query Tool");
        smallProject.set("description", "A tool to help accounting query various things.");
        return smallProject;
    }

    @SuppressWarnings("unchecked")
    private void addManagedEmployees(int managerIndex, int[] employeeIndeces) {
        DynamicEntity manager = this.employees[managerIndex];

        if (manager.<Collection> get("managedEmployees").isEmpty()) {
            for (int index = 0; index < employeeIndeces.length; index++) {
                manager.<Collection> get("managedEmployees").add(this.employees[employeeIndeces[index]]);
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void addProjects(int empIndex, int[] smallProjIndeces, int[] largeProjIndeces) {
        DynamicEntity employee = this.employees[empIndex];
        Collection<DynamicEntity> projects = employee.<Collection> get("projects");

        for (int index = 0; index < smallProjIndeces.length; index++) {
            projects.add(this.smallProjects[smallProjIndeces[index]]);
        }

        for (int index = 0; index < largeProjIndeces.length; index++) {
            projects.add(this.largeProjects[largeProjIndeces[index]]);
        }
    }

    /**
     * Register all of the population in the provided EntityManager to be
     * persisted This method should only be called from within a test case. It
     * asserts that the provided EntityManager is in a transaction and that the
     * database tables are empty.
     */
    public void persistAll(EntityManager em) {
        Assert.assertTrue("EntityManager not in Transaction", em.getTransaction().isActive());

        // Verify that the database tables are empty
        assertCount(em, "Employee", 0);
        assertCount(em, "Address", 0);
        assertCount(em, "PhoneNumber", 0);
        assertCount(em, "Project", 0);

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
        verifyCounts(em);
    }

    public void verifyCounts(EntityManager em) {
        assertCount(em, "Employee", this.employees.length);
        assertCount(em, "Address", this.employees.length);
        assertCount(em, "Project", this.smallProjects.length + this.largeProjects.length);
    }

    /**
     * Verify that the provided entity type has no rows in the database using a
     * native ReportQuery.
     * 
     * @param entityClass
     * @param count
     */
    public void assertCount(EntityManager em, String entityAlias, int count) {
        Class<?> entityClass = getDynamicClass(entityAlias);
        ReportQuery query = new ReportQuery(entityClass, new ExpressionBuilder());
        query.addCount();
        query.setShouldReturnSingleValue(true);

        int dbCount = ((Number) JpaHelper.getEntityManager(em).getUnitOfWork().executeQuery(query)).intValue();
        Assert.assertEquals("Incorrect quantity found of " + entityClass, count, dbCount);
    }

    /**
     * Verify that the provided list of Employee instances matches the sample
     * population.
     * 
     * @param employees
     */
    public void assertSame(List<DynamicEntity> dbEmps) {
        Assert.assertEquals("Incorrect quantity of employees", this.employees.length, dbEmps.size());
        Collections.sort(dbEmps, new DynamicEntityComparator());

        List<DynamicEntity> sampleEmps = new ArrayList<DynamicEntity>();
        for (int index = 0; index < this.employees.length; index++) {
            sampleEmps.add(this.employees[index]);
        }
        Collections.sort(sampleEmps, new DynamicEntityComparator());

        for (int index = 0; index < this.employees.length; index++) {
            DynamicEntity emp = sampleEmps.get(index);
            DynamicEntity dbEmp = dbEmps.get(index);

            Assert.assertEquals("First name does not match on employees[" + index + "]", emp.<String> get("firstName"), dbEmp.<String> get("firstName"));
            Assert.assertEquals("Last name does not match on employees[" + index + "]", emp.<String> get("lastName"), dbEmp.<String> get("lastName"));
            Assert.assertEquals("Salary does not match on employees[" + index + "]", emp.<Integer> get("salary"), dbEmp.<Integer> get("salary"));
        }
    }

    /**
     * Simple comparator used to order the employees for use within assertSame
     */
    class DynamicEntityComparator implements Comparator<DynamicEntity> {

        public int compare(DynamicEntity emp1, DynamicEntity emp2) {
            return emp1.<Integer> get("id") - emp2.<Integer> get("id");
        }

    }

    /**
     * Extract the id's from the sample Employee instances.
     * 
     * @param em
     * @return
     */
    public int[] getEmployeeIds(EntityManager em) {
        int[] ids = new int[this.employees.length];

        for (int index = 0; index < this.employees.length; index++) {
            ids[index] = this.employees[index].<Integer> get("id");
        }

        return ids;
    }

    /**
     * Reset the database so that only the sample population exists.
     * 
     * @param em
     */
    public void resetDatabase(EntityManager em) {
        em.getTransaction().begin();

        DeleteAllQuery deleteEmpsQuery = new DeleteAllQuery(getDynamicClass("Employee"));
        ExpressionBuilder eb = deleteEmpsQuery.getExpressionBuilder();
        deleteEmpsQuery.setSelectionCriteria(eb.get("id").notIn(getEmployeeIds(em)));
        deleteEmpsQuery.setFlushOnExecute(true);

        JpaHelper.getEntityManager(em).getUnitOfWork().executeQuery(deleteEmpsQuery);

        em.getTransaction().commit();
    }

    public void resetSalary(EntityManager em) {
        boolean startedTX = !em.getTransaction().isActive();

        if (startedTX) {
            em.getTransaction().begin();
        }

        for (int index = 0; index < this.employees.length; index++) {
            DynamicEntity emp = this.employees[index];
            Query query = em.createQuery("SELECT e FROM Employee e WHERE e.firstName = :FNAME AND e.lastName = :LNAME");
            query.setParameter("FNAME", emp.<String> get("firstName"));
            query.setParameter("LNAME", emp.<String> get("lastName"));

            DynamicEntity dbEmp = (DynamicEntity) query.getSingleResult();
            dbEmp.set("salary", this.employees[index].<Integer> get("salary"));
        }

        if (startedTX) {
            em.getTransaction().commit();
        }
    }
}
