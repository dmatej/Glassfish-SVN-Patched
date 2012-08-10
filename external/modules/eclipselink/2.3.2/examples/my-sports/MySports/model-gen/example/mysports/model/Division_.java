package example.mysports.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.ListAttribute;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2011-05-25T03:14:05.700-0400")
@StaticMetamodel(Division.class)
public class Division_ {
	public static volatile SingularAttribute<Division, Integer> id;
	public static volatile SingularAttribute<Division, Boolean> defaultDivision;
	public static volatile SingularAttribute<Division, String> name;
	public static volatile ListAttribute<Division, Team> teams;
	public static volatile SingularAttribute<Division, Long> version;
}
