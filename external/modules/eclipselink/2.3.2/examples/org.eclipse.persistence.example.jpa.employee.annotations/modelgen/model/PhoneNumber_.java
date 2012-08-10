package model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-06-06T13:46:45.437-0400")
@StaticMetamodel(PhoneNumber.class)
public class PhoneNumber_ {
	public static volatile SingularAttribute<PhoneNumber, Integer> id;
	public static volatile SingularAttribute<PhoneNumber, String> type;
	public static volatile SingularAttribute<PhoneNumber, String> areaCode;
	public static volatile SingularAttribute<PhoneNumber, String> number;
	public static volatile SingularAttribute<PhoneNumber, Employee> owner;
}
