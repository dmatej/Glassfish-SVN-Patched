package model;

import java.sql.Time;
import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-06-07T09:42:48.562-0400")
@StaticMetamodel(Employee.class)
public class Employee_ {
	public static volatile SingularAttribute<Employee, Integer> id;
	public static volatile SingularAttribute<Employee, String> firstName;
	public static volatile SingularAttribute<Employee, String> lastName;
	public static volatile SingularAttribute<Employee, Time> startTime;
	public static volatile SingularAttribute<Employee, Time> endTime;
	public static volatile SingularAttribute<Employee, Gender> gender;
	public static volatile SingularAttribute<Employee, Double> salary;
	public static volatile SingularAttribute<Employee, Long> version;
	public static volatile SingularAttribute<Employee, Employee> manager;
	public static volatile ListAttribute<Employee, Employee> managedEmployees;
	public static volatile ListAttribute<Employee, PhoneNumber> phoneNumbers;
	public static volatile SingularAttribute<Employee, Address> address;
	public static volatile ListAttribute<Employee, Project> projects;
	public static volatile ListAttribute<Employee, Object> responsibilities;
	public static volatile SingularAttribute<Employee, EmploymentPeriod> period;
}
