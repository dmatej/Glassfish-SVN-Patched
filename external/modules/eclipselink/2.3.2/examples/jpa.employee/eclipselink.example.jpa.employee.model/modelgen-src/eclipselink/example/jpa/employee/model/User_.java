package eclipselink.example.jpa.employee.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2011-03-28T12:58:00.236-0400")
@StaticMetamodel(User.class)
public class User_ {
	public static volatile SingularAttribute<User, String> id;
	public static volatile SingularAttribute<User, String> password;
	public static volatile SingularAttribute<User, String> firstName;
	public static volatile SingularAttribute<User, String> lastName;
	public static volatile SingularAttribute<User, Company> company;
	public static volatile SingularAttribute<User, Boolean> administrator;
	public static volatile SingularAttribute<User, Boolean> companyAdmin;
}
