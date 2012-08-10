package eclipselink.example.jpa.employee.model;

import java.sql.Date;
import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2011-04-08T13:42:05.083-0400")
@StaticMetamodel(Project.class)
public class Project_ {
	public static volatile SingularAttribute<Project, Integer> id;
	public static volatile SingularAttribute<Project, String> name;
	public static volatile SingularAttribute<Project, String> description;
	public static volatile SingularAttribute<Project, Date> startDate;
	public static volatile SingularAttribute<Project, Date> endDate;
	public static volatile SingularAttribute<Project, Long> version;
	public static volatile ListAttribute<Project, Member> members;
}
