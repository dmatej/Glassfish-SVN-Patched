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

import java.lang.reflect.GenericArrayType;
import java.lang.reflect.Type;

/**
 * Default implementation of a generic array type.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultGenericArrayType.java 2 2009-02-02 22:28:39Z markhobson $
 * @see GenericArrayType
 */
class DefaultGenericArrayType implements GenericArrayType
{
	// TODO: make serializable?
	
	// fields -----------------------------------------------------------------
	
	private final Type componentType;
	
	// constructors -----------------------------------------------------------
	
	public DefaultGenericArrayType(Type componentType)
	{
		this.componentType = Utils.checkNotNull(componentType, "componentType");
	}
	
	// GenericArrayType methods -----------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	public Type getGenericComponentType()
	{
		return componentType;
	}
	
	// Object methods ---------------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode()
	{
		return componentType.hashCode();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object object)
	{
		if (!(object instanceof GenericArrayType))
		{
			return false;
		}
		
		GenericArrayType type = (GenericArrayType) object;
		
		return componentType.equals(type.getGenericComponentType());
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
	
	public static String toString(GenericArrayType type)
	{
		return toString(type, ClassSerializers.QUALIFIED);
	}
	
	public static String toString(GenericArrayType type, ClassSerializer serializer)
	{
		return TypeUtils.toString(type.getGenericComponentType(), serializer) + "[]";
	}
}
