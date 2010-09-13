/*
 * Copyright 2009 IIZUKA Software Technologies Ltd
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.googlecode.jtype;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;

/**
 * Default implementation of a type variable.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultTypeVariable.java 2 2009-02-02 22:28:39Z markhobson $
 * @param <D>
 *            the type of generic declaration that declared the type variable
 * @see TypeVariable
 */
class DefaultTypeVariable<D extends GenericDeclaration> implements TypeVariable<D>
{
	// TODO: make serializable?
	
	// constants --------------------------------------------------------------
	
	private static final Type[] DEFAULT_BOUNDS = new Type[] {Object.class};
	
	// fields -----------------------------------------------------------------
	
	private final D declaration;
	
	private final String name;
	
	private final Type[] bounds;
	
	// constructors -----------------------------------------------------------
	
	public DefaultTypeVariable(D declaration, String name, Type[] bounds)
	{
		if (bounds == null || bounds.length == 0)
		{
			bounds = DEFAULT_BOUNDS;
		}
		
		this.declaration = Utils.checkNotNull(declaration, "declaration");
		this.name = Utils.checkNotNull(name, "name");
		this.bounds = bounds.clone();
	}
	
	// TypeVariable methods ---------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	public D getGenericDeclaration()
	{
		return declaration;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public String getName()
	{
		return name;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public Type[] getBounds()
	{
		return bounds.clone();
	}
	
	// Object methods ---------------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode()
	{
		int hashCode = declaration.hashCode();
		hashCode = (hashCode * 37) + name.hashCode();
		hashCode = (hashCode * 37) + Arrays.hashCode(bounds);
		
		return hashCode;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object object)
	{
		if (!(object instanceof TypeVariable))
		{
			return false;
		}
		
		TypeVariable<?> typeVariable = (TypeVariable<?>) object;
		
		return declaration.equals(typeVariable.getGenericDeclaration())
			&& name.equals(typeVariable.getName())
			&& Arrays.equals(bounds, typeVariable.getBounds());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString()
	{
		return toString(this);
	}
	
	// public methods ---------------------------------------------------------
	
	public static String toString(TypeVariable<?> type)
	{
		return toString(type, ClassSerializers.QUALIFIED);
	}
	
	public static String toString(TypeVariable<?> type, ClassSerializer serializer)
	{
		StringBuilder builder = new StringBuilder();
		
		builder.append(type.getName());
		
		if (!Arrays.equals(DEFAULT_BOUNDS, type.getBounds()))
		{
			builder.append(" extends ");
			
			TypeUtils.appendBounds(builder, type.getBounds(), serializer);
		}
		
		return builder.toString();
	}
}
