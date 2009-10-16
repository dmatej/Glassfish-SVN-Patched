//$Id: BoneEater.java 15765 2009-01-09 14:56:30Z hardy.ferentschik $
package org.hibernate.validator.test.inheritance;

import org.hibernate.validator.NotNull;

/**
 * @author Emmanuel Bernard
 */
public interface BoneEater extends Eater {
	@NotNull
	String getFavoriteBone();

	void setFavoriteBone(String favoriteBone);
}
