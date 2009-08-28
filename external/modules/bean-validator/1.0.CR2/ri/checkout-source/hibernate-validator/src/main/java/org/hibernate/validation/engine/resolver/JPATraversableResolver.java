// $Id: JPATraversableResolver.java 16143 2009-03-11 13:09:25Z hardy.ferentschik $
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
package org.hibernate.validation.engine.resolver;

import java.lang.annotation.ElementType;
import javax.persistence.Persistence;
import javax.validation.TraversableResolver;

/**
 * @author Hardy Ferentschik
 */
public class JPATraversableResolver implements TraversableResolver {

	public boolean isTraversable(Object traversableObject, String traversableProperty, Class<?> rootBeanType, String pathToTraversableObject, ElementType elementType) {
	    //		return Persistence.getPersistenceUtil().isLoaded( traversableObject, traversableProperty );

	    // edburns return false, because this code will not be
	    // called when running on glassfish, only on
	    return false;
	}
}
