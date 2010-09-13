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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import java.lang.reflect.GenericArrayType;

import org.junit.Test;

/**
 * Tests {@code DefaultGenericArrayType}.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultGenericArrayTypeTest.java 2 2009-02-02 22:28:39Z markhobson $
 * @see DefaultGenericArrayType
 */
public class DefaultGenericArrayTypeTest
{
	// tests ------------------------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void constructorWithNullComponentType()
	{
		try
		{
			new DefaultGenericArrayType(null);
		}
		catch (NullPointerException exception)
		{
			assertEquals("componentType cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test
	public void constructorWithComponentType()
	{
		GenericArrayType genericArrayType = new DefaultGenericArrayType(Integer.class);
		
		assertEquals(Integer.class, genericArrayType.getGenericComponentType());
	}
	
	@Test
	public void hashCodeTest()
	{
		GenericArrayType genericArrayType1 = new DefaultGenericArrayType(Integer.class);
		GenericArrayType genericArrayType2 = new DefaultGenericArrayType(Integer.class);
		
		assertEquals(genericArrayType1.hashCode(), genericArrayType2.hashCode());
	}
	
	@Test
	public void equalsWhenEqual()
	{
		GenericArrayType genericArrayType1 = new DefaultGenericArrayType(Integer.class);
		GenericArrayType genericArrayType2 = new DefaultGenericArrayType(Integer.class);
		
		assertEquals(genericArrayType1, genericArrayType2);
	}
	
	@Test
	public void equalsWithDifferentClass()
	{
		GenericArrayType genericArrayType = new DefaultGenericArrayType(Integer.class);
		
		assertFalse(genericArrayType.equals(new Object()));
	}
	
	@Test
	public void equalsWithDifferentComponentTypes()
	{
		GenericArrayType genericArrayType1 = new DefaultGenericArrayType(Integer.class);
		GenericArrayType genericArrayType2 = new DefaultGenericArrayType(Number.class);
		
		assertFalse(genericArrayType1.equals(genericArrayType2));
	}
	
	@Test
	public void toStringTest()
	{
		GenericArrayType genericArrayType = new DefaultGenericArrayType(Integer.class);
		
		assertEquals("java.lang.Integer[]", genericArrayType.toString());
	}
}
