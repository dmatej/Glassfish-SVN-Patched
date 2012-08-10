package eclipselink.example.jpa.employee.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2011-04-11T12:29:42.516-0400")
@StaticMetamodel(PhoneNumber.class)
public class PhoneNumber_ {
	public static volatile SingularAttribute<PhoneNumber, String> type;
	public static volatile SingularAttribute<PhoneNumber, String> areaCode;
	public static volatile SingularAttribute<PhoneNumber, String> number;
	public static volatile SingularAttribute<PhoneNumber, Employee> owner;
	public static volatile SingularAttribute<PhoneNumber, Integer> id;
}
