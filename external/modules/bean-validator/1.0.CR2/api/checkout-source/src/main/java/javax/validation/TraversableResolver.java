// $Id: TraversableResolver.java 16125 2009-03-10 03:52:30Z epbernard $
/*
* JBoss, Home of Professional Open Source
* Copyright 2008, Red Hat Middleware LLC, and individual contributors
* by the @authors tag. See the copyright.txt in the distribution for a
* full listing of individual contributors.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
* http://www.apache.org/licenses/LICENSE-2.0
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package javax.validation;

import java.lang.annotation.ElementType;

/**
 * Contract determining if a property can be accessed by the Bean Validation provider
 * This contract is called for each property either validated or traversed.
 *
 * A traversable resolver implementation must me thread-safe.
 *
 * @author Emmanuel Bernard
 */
public interface TraversableResolver {
	/**
	 * Determine if a property can be traversed by Bean Validation.
	 *
	 * @param traversableObject object hosting <code>traversableProperty</code>.
	 * @param traversableProperty name of the traversable property.
	 * @param rootBeanType type of the root object passed to the Validator.
	 * @param pathToTraversableObject path from the root object to
	 *        the <code>traversableProperty</code>
	 *        (using the path specification defined by Bean Validator).
	 * @param elementType either <code>FIELD</code> or <code>METHOD</code>.
	 *
	 * @return <code>true</code> if the property is traversable by Bean Validation,
	 *         <code>false</code> otherwise.
	 */
	boolean isTraversable(Object traversableObject,
						  String traversableProperty,
						  Class<?> rootBeanType,
						  String pathToTraversableObject,
						  ElementType elementType);
}
