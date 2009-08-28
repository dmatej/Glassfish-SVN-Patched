// $Id: ConstraintValidatorContext.java 16159 2009-03-15 15:39:59Z epbernard $
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

/**
 * Provide contextual data and operation when applying a given constraint validator
 *
 * @author Emmanuel Bernard
 */
public interface ConstraintValidatorContext {
	/**
	 * Disable the default error message and default ConstraintViolation object generation.
	 * Useful to set a different error message or generate a ConstraintViolation based on
	 * a different property
	 *
	 * @see #addError(String)
	 * @see #addError(String, String)
	 */
	void disableDefaultError();

	/**
	 * @return the current uninterpolated default message
	 */
	String getDefaultErrorMessage();

	/**
	 * Add a new error message. This error message will be interpolated.
	 * <p/>
	 * If <code>isValid<code> returns <code>false</code>, a <code>ConstraintViolation</code> object will be built
	 * per error message including the default one unless {@link #disableDefaultError()} has been called.
	 * <p/>
	 * Aside from the error message, <code>ConstraintViolation</code> objects generated from such a call
	 * contain the same contextual information (root bean, path and so on)
	 * <p/>
	 * This method can be called multiple times. One <code>ConstraintViolation</code> instance per
	 * call is created.
	 *
	 * @param message new uninterpolated error message.
	 */
	void addError(String message);

	/**
	 * Add a new error message to a given sub property <code>property</code>. The subproperty
	 * is relative to the path tot he bean or property hosting the constraint.
	 *
	 * This error message will be interpolated.
	 * <p/>
	 * If <code>isValid</code> returns <code>false</code>, a <code>ConstraintViolation</code> object will be built
	 * per error message including the default one unless {@link #disableDefaultError()}
	 * has been called.
	 * <p/>
	 * Aside from the error message and the property path, <code>ConstraintViolation</code> objects
	 * generated from such a call contain the same contextual information
	 * (root bean, leaf bean etc)
	 * <p/>
	 * This method can be called multiple times. One <code>ConstraintViolation</code> instance per
	 * call is created.
	 *
	 * @param message new uninterpolated error message.
	 * @param property property name the </code>ConstraintViolation</code> is targeting.
	 */
	void addError(String message, String property);
}
