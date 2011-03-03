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

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.util.Arrays;

/**
 * Default implementation of a wildcard type.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultWildcardType.java 2 2009-02-02 22:28:39Z markhobson $
 * @see WildcardType
 */
class DefaultWildcardType implements WildcardType
{
	// TODO: make serializable?
	
	// constants --------------------------------------------------------------
	
	private static final Type[] DEFAULT_UPPER_BOUNDS = new Type[] {Object.class};
	
	private static final Type[] DEFAULT_LOWER_BOUNDS = new Type[0];
	
	// fields -----------------------------------------------------------------
	
	private final Type[] upperBounds;
	
	private final Type[] lowerBounds;
	
	// constructors -----------------------------------------------------------
	
	public DefaultWildcardType(Type[] upperBounds, Type[] lowerBounds)
	{
		if (upperBounds == null || upperBounds.length == 0)
		{
			upperBounds = DEFAULT_UPPER_BOUNDS;
		}
		
		if (lowerBounds == null)
		{
			lowerBounds = DEFAULT_LOWER_BOUNDS;
		}
		
		this.upperBounds = upperBounds.clone();
		this.lowerBounds = lowerBounds.clone();
		
		boolean hasUpperBounds = !Arrays.equals(this.upperBounds, DEFAULT_UPPER_BOUNDS);
		boolean hasLowerBounds = !Arrays.equals(this.lowerBounds, DEFAULT_LOWER_BOUNDS);
		Utils.checkFalse(hasUpperBounds && hasLowerBounds, "Wildcard type cannot have both upper and lower bounds");
	}
	
	// WildcardType methods ---------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	public Type[] getUpperBounds()
	{
		return upperBounds.clone();
	}
	
	/**
	 * {@inheritDoc}
	 */
	public Type[] getLowerBounds()
	{
		return lowerBounds.clone();
	}
	
	// Object methods ---------------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode()
	{
		return Arrays.hashCode(lowerBounds) ^ Arrays.hashCode(upperBounds);
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object object)
	{
		if (!(object instanceof WildcardType))
		{
			return false;
		}
		
		WildcardType wildcardType = (WildcardType) object;
		
		return Arrays.equals(lowerBounds, wildcardType.getLowerBounds())
			&& Arrays.equals(upperBounds, wildcardType.getUpperBounds());
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
	
	public static String toString(WildcardType type)
	{
		return toString(type, ClassSerializers.QUALIFIED);
	}
	
	public static String toString(WildcardType type, ClassSerializer serializer)
	{
		StringBuilder builder = new StringBuilder();
		
		builder.append("?");
		
		Type[] lowerBounds = type.getLowerBounds();
		Type[] upperBounds = type.getUpperBounds();
		
		if (!Arrays.equals(DEFAULT_LOWER_BOUNDS, lowerBounds))
		{
			builder.append(" super ");
			
			TypeUtils.appendBounds(builder, lowerBounds, serializer);
		}
		else if (!Arrays.equals(DEFAULT_UPPER_BOUNDS, upperBounds))
		{
			builder.append(" extends ");
			
			TypeUtils.appendBounds(builder, upperBounds, serializer);
		}
		
		return builder.toString();
	}
}
