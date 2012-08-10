package example.mysports.model;

import javax.annotation.Generated;
import javax.persistence.metamodel.SingularAttribute;
import javax.persistence.metamodel.StaticMetamodel;

@Generated(value="Dali", date="2011-05-25T02:49:06.872-0400")
@StaticMetamodel(Player.class)
public class Player_ {
	public static volatile SingularAttribute<Player, Integer> id;
	public static volatile SingularAttribute<Player, String> userid;
	public static volatile SingularAttribute<Player, String> firstName;
	public static volatile SingularAttribute<Player, String> lastName;
	public static volatile SingularAttribute<Player, String> email;
	public static volatile SingularAttribute<Player, Team> team;
	public static volatile SingularAttribute<Player, Integer> number;
	public static volatile SingularAttribute<Player, Long> version;
}
