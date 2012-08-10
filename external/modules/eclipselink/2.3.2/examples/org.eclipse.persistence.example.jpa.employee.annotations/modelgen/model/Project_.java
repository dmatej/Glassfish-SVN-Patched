package model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-06-06T13:46:45.437-0400")
@StaticMetamodel(Project.class)
public class Project_ {
	public static volatile SingularAttribute<Project, Integer> id;
	public static volatile SingularAttribute<Project, String> name;
	public static volatile SingularAttribute<Project, String> description;
	public static volatile SingularAttribute<Project, Long> version;
	public static volatile SingularAttribute<Project, Employee> teamLeader;
}
