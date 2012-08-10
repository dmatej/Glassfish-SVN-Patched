package eclipselink.example.jpa.employee.model;

import java.util.Calendar;
import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2010-09-20T14:23:56.562-0400")
@StaticMetamodel(LargeProject.class)
public class LargeProject_ extends Project_ {
	public static volatile SingularAttribute<LargeProject, Double> budget;
	public static volatile SingularAttribute<LargeProject, Calendar> milestone;
}
