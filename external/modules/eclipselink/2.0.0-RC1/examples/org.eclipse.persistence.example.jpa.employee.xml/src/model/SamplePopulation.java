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
 * 		dclarke - initial JPA Employee example using XML (bug 217884)
 ******************************************************************************/
package model;

import javax.persistence.EntityManager;

public class SamplePopulation {

	private Employee[] employees = { basicEmployeeExample1(),
			basicEmployeeExample2(), basicEmployeeExample3(),
			basicEmployeeExample4(), basicEmployeeExample5(),
			basicEmployeeExample6(), basicEmployeeExample7(),
			basicEmployeeExample8(), basicEmployeeExample9(),
			basicEmployeeExample10(), basicEmployeeExample11(),
			basicEmployeeExample12() };

	private SmallProject[] smallProjects = { basicSmallProjectExample1(),
			basicSmallProjectExample2(), basicSmallProjectExample3(),
			basicSmallProjectExample4(), basicSmallProjectExample5(),
			basicSmallProjectExample7(), basicSmallProjectExample8(),
			basicSmallProjectExample9(), basicSmallProjectExample10() };

	private LargeProject[] largeProjects = { basicLargeProjectExample1(),
			basicLargeProjectExample2(), basicLargeProjectExample3(),
			basicLargeProjectExample4(), basicLargeProjectExample5() };

	public SamplePopulation() {

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
		addManagedEmployees(9, new int[] { 11 });
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
		this.largeProjects[0].setTeamLeader(this.employees[1]);
		this.largeProjects[3].setTeamLeader(this.employees[2]);
		this.largeProjects[4].setTeamLeader(this.employees[2]);
	}

	public Employee basicEmployeeExample1() {
		Employee employee = new Employee();

		employee.setFirstName("Bob");
		employee.setLastName("Smith");
		employee.setGender(Gender.Male);
		employee.setSalary(35000);

		EmploymentPeriod employmentPeriod = new EmploymentPeriod();
		employmentPeriod.setEndDate(1996, 0, 1);
		employmentPeriod.setStartDate(1993, 0, 1);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Toronto");
		address.setPostalCode("L5J2B5");
		address.setProvince("ONT");
		address.setStreet("1450 Acme Cr., Suite 4");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee.addResponsibility("Water the office plants.");
		employee.addResponsibility("Maintain the kitchen facilities.");
		employee.addPhoneNumber("Work", "613", "5558812");

		return employee;
	}

	public Employee basicEmployeeExample10() {
		Employee employee = new Employee();

		employee.setFirstName("Jill");
		employee.setLastName("May");
		employee.setGender(Gender.Female);

		EmploymentPeriod employmentPeriod = new EmploymentPeriod();
		employmentPeriod.setStartDate(1991, 10, 11);
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
		employmentPeriod.setEndDate(1996, 0, 1);
		employmentPeriod.setStartDate(1993, 0, 1);
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
		employmentPeriod.setEndDate(2001, 11, 31);
		employmentPeriod.setStartDate(1995, 0, 12);
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
		employmentPeriod.setStartDate(1991, 10, 11);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Ottawa");
		address.setPostalCode("K5J2B5");
		address.setProvince("ONT");
		address.setStreet("12 Merivale Rd., Suite 5");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee
				.addResponsibility("Hire people when more people are required.");
		employee
				.addResponsibility("Lay off employees when less people are required.");
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
		employmentPeriod.setEndDate(2001, 11, 31);
		employmentPeriod.setStartDate(1995, 0, 1);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Montreal");
		address.setPostalCode("Q2S5Z5");
		address.setProvince("QUE");
		address.setStreet("1 Canadien Place");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee.addResponsibility("Perform code reviews as required.");
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
		employmentPeriod.setEndDate(2001, 11, 31);
		employmentPeriod.setStartDate(1995, 0, 1);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Vancouver");
		address.setPostalCode("N5J2N5");
		address.setProvince("BC");
		address.setStreet("20 Mountain Blvd., Floor 53, Suite 6");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee.addResponsibility("Have to fix the Database problem.");
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
		employmentPeriod.setEndDate(2001, 6, 31);
		employmentPeriod.setStartDate(1995, 4, 1);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Prince Rupert");
		address.setPostalCode("K3K5D5");
		address.setProvince("BC");
		address.setStreet("3254 Parkway Place");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee.addResponsibility("Write code documentation.");
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
		employmentPeriod.setEndDate(2001, 11, 31);
		employmentPeriod.setStartDate(1995, 0, 12);
		employee.setPeriod(employmentPeriod);

		Address address = new Address();
		address.setCity("Perth");
		address.setPostalCode("Y3Q2N9");
		address.setProvince("ONT");
		address.setStreet("234 Caledonia Lane");
		address.setCountry("Canada");
		employee.setAddress(address);

		employee.addResponsibility("Write user specifications.");
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
		employmentPeriod.setEndDate(1996, 0, 1);
		employmentPeriod.setStartDate(1993, 0, 1);
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
		employmentPeriod.setEndDate(2001, 11, 31);
		employmentPeriod.setStartDate(1995, 0, 1);
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
		employmentPeriod.setStartDate(2001, 11, 31);
		employmentPeriod.setEndDate(1995, 0, 1);
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
		largeProject
				.setDescription("A reporting application to report on the corporations database through TopLink.");
		largeProject.setBudget((double) 5000);
		largeProject.getMilestone().set(1991, 10, 11, 12, 0, 0);

		return largeProject;
	}

	public LargeProject basicLargeProjectExample2() {
		LargeProject largeProject = new LargeProject();

		largeProject.setName("Light Reporter");
		largeProject
				.setDescription("A lightweight application to report on the corporations database through TopLink.");
		largeProject.setBudget(100.98);
		largeProject.getMilestone().set(1999, 11, 25, 11, 40, 44);

		return largeProject;
	}

	public LargeProject basicLargeProjectExample3() {
		LargeProject largeProject = new LargeProject();

		largeProject.setName("TOPEmployee Management");
		largeProject
				.setDescription("A management application to report on the corporations database through TopLink.");
		largeProject.setBudget(4000.98);
		largeProject.getMilestone().set(1997, 10, 12, 1, 0, 0);

		return largeProject;
	}

	public LargeProject basicLargeProjectExample4() {
		LargeProject largeProject = new LargeProject();

		largeProject.setName("Enterprise System");
		largeProject
				.setDescription("A enterprise wide application to report on the corporations database through TopLink.");
		largeProject.setBudget(40.98);
		largeProject.getMilestone().set(1996, 8, 6, 6, 40, 44);

		return largeProject;
	}

	public LargeProject basicLargeProjectExample5() {
		LargeProject largeProject = new LargeProject();

		largeProject.setName("Problem Reporting System");
		largeProject
				.setDescription("A PRS application to report on the corporations database through TopLink.");
		largeProject.setBudget(101.98);
		largeProject.getMilestone().set(1997, 9, 6, 1, 40, 44);

		return largeProject;
	}

	public SmallProject basicSmallProjectExample1() {
		return new SmallProject(
				"Enterprise",
				"A enterprise wide application to report on the corporations database through TopLink.");
	}

	public SmallProject basicSmallProjectExample10() {
		return new SmallProject("Staff Query Tool",
				"A tool to help staff query various things.");
	}

	public SmallProject basicSmallProjectExample2() {
		return new SmallProject(
				"Sales Reporter",
				"A reporting application using JDK to report on the corporations database through TopLink.");
	}

	public SmallProject basicSmallProjectExample3() {
		return new SmallProject(
				"TOP-Employee Manager",
				"A management application to report on the corporations database through TopLink.");
	}

	public SmallProject basicSmallProjectExample4() {
		return new SmallProject("Problem Reporter",
				"A PRS application to report on the corporations database through TopLink.");
	}

	public SmallProject basicSmallProjectExample5() {
		return new SmallProject(
				"Feather Reporter",
				"An extremely lightweight application to report on the corporations database through TopLink.");
	}

	public SmallProject basicSmallProjectExample6() {
		return new SmallProject("Makework", "A makework project.");
	}

	public SmallProject basicSmallProjectExample7() {
		return new SmallProject("Marketing Query Tool",
				"A tool to help marketing query various things.");
	}

	public SmallProject basicSmallProjectExample8() {
		return new SmallProject("Shipping Query Tool",
				"A tool to help shipping query various things.");
	}

	public SmallProject basicSmallProjectExample9() {
		return new SmallProject("Accounting Query Tool",
				"A tool to help accounting query various things.");
	}

	private void addManagedEmployees(int managerIndex, int[] employeeIndeces) {
		Employee manager = this.employees[managerIndex];

		if (manager.getManagedEmployees().isEmpty()) {
			for (int index = 0; index < employeeIndeces.length; index++) {
				manager
						.addManagedEmployee(this.employees[employeeIndeces[index]]);
			}
		}
	}

	private void addProjects(int empIndex, int[] smallProjIndeces,
			int[] largeProjIndeces) {
		Employee employee = this.employees[empIndex];

		for (int index = 0; index < smallProjIndeces.length; index++) {
			employee.addProject(this.smallProjects[smallProjIndeces[index]]);
		}

		for (int index = 0; index < largeProjIndeces.length; index++) {
			employee.addProject(this.largeProjects[largeProjIndeces[index]]);
		}
	}

	/**
	 * 
	 * @param em
	 */
	public void persistAll(EntityManager em) {
		for (int index = 0; index < this.employees.length; index++) {
			em.persist(this.employees[index]);
		}
		for (int index = 0; index < this.smallProjects.length; index++) {
			em.persist(this.smallProjects[index]);
		}
		for (int index = 0; index < this.largeProjects.length; index++) {
			em.persist(this.largeProjects[index]);
		}
	}
}
