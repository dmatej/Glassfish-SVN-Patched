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

import java.lang.reflect.MalformedParameterizedTypeException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.util.Arrays;

/**
 * Default implementation of a parameterized type.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultParameterizedType.java 2 2009-02-02 22:28:39Z markhobson $
 * @see ParameterizedType
 */
class DefaultParameterizedType implements ParameterizedType
{
	// TODO: make serializable?
	
	// fields -----------------------------------------------------------------
	
	private final Type ownerType;
	
	private final Type rawType;
	
	private final Type[] actualTypeArguments;
	
	// constructors -----------------------------------------------------------
	
	public DefaultParameterizedType(Type ownerType, Class<?> rawType, Type[] actualTypeArguments)
	{
		this.rawType = Utils.checkNotNull(rawType, "rawType");
		
		if (actualTypeArguments == null)
		{
			actualTypeArguments = new Type[0];
		}
		
		TypeVariable<?>[] typeParameters = rawType.getTypeParameters();
		
		if (typeParameters.length != actualTypeArguments.length)
		{
			throw new MalformedParameterizedTypeException();
		}
		
		if (ownerType == null)
		{
			ownerType = rawType.getDeclaringClass();
		}
		
		this.ownerType = ownerType;
		this.actualTypeArguments = actualTypeArguments.clone();
	}
	
	// ParameterizedType methods ----------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	public Type getOwnerType()
	{
		return ownerType;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public Type getRawType()
	{
		return rawType;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public Type[] getActualTypeArguments()
	{
		return actualTypeArguments.clone();
	}
	
	// Object methods ---------------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode()
	{
		int hashCode = Utils.nullHashCode(ownerType);
		hashCode = (37 * hashCode) + rawType.hashCode();
		hashCode = (37 * hashCode) + Arrays.hashCode(actualTypeArguments);
		
		return hashCode;
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object object)
	{
		if (!(object instanceof ParameterizedType))
		{
			return false;
		}
		
		ParameterizedType type = (ParameterizedType) object;
		
		if (type == this)
		{
			return true;
		}
		
		return Utils.nullEquals(ownerType, type.getOwnerType())
			&& rawType.equals(type.getRawType())
			&& Arrays.equals(actualTypeArguments, type.getActualTypeArguments());
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
	
	public static String toString(ParameterizedType type)
	{
		return toString(type, ClassSerializers.QUALIFIED);
	}
	
	public static String toString(ParameterizedType type, ClassSerializer serializer)
	{
		StringBuilder builder = new StringBuilder();
		
		Type ownerType = type.getOwnerType();
		
		if (ownerType != null)
		{
			builder.append(TypeUtils.toString(ownerType, serializer));
			builder.append(".");
		}
		
		builder.append(TypeUtils.toString(type.getRawType(), serializer));
		
		Type[] actualTypeArguments = type.getActualTypeArguments();
		
		if (actualTypeArguments != null && actualTypeArguments.length > 0)
		{
			builder.append("<");
			
			for (int i = 0; i < actualTypeArguments.length; i++)
			{
				if (i > 0)
				{
					builder.append(",");
				}
				
				builder.append(TypeUtils.toString(actualTypeArguments[i], serializer));
			}
			
			builder.append(">");
		}
		
		return builder.toString();
	}
}
