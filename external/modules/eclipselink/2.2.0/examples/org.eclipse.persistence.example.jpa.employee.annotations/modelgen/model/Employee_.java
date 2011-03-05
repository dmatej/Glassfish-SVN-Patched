package model;

import java.util.List;
import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-06-06T13:46:45.390-0400")
@StaticMetamodel(Employee.class)
public class Employee_ {
	public static volatile SingularAttribute<Employee, Integer> id;
	public static volatile SingularAttribute<Employee, String> firstName;
	public static volatile SingularAttribute<Employee, Gender> gender;
	public static volatile SingularAttribute<Employee, String> lastName;
	public static volatile SingularAttribute<Employee, Double> salary;
	public static volatile SingularAttribute<Employee, Long> version;
	public static volatile ListAttribute<Employee, Project> projects;
	public static volatile SingularAttribute<Employee, Employee> manager;
	public static volatile ListAttribute<Employee, Employee> managedEmployees;
	public static volatile ListAttribute<Employee, PhoneNumber> phoneNumbers;
	public static volatile SingularAttribute<Employee, Address> address;
	public static volatile SingularAttribute<Employee, EmploymentPeriod> period;
	public static volatile SingularAttribute<Employee, List> responsibilities;
}
