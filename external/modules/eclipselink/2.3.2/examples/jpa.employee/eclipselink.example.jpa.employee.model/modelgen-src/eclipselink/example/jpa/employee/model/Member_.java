package eclipselink.example.jpa.employee.model;

import java.sql.Timestamp;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-09-20T14:23:56.562-0400")
@StaticMetamodel(Member.class)
public class Member_ {
	public static volatile SingularAttribute<Member, Integer> id;
	public static volatile SingularAttribute<Member, Project> project;
	public static volatile SingularAttribute<Member, Employee> employee;
	public static volatile SingularAttribute<Member, Timestamp> start;
	public static volatile SingularAttribute<Member, Timestamp> end;
	public static volatile SingularAttribute<Member, Boolean> lead;
}
