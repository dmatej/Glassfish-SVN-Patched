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

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;

import java.lang.reflect.MalformedParameterizedTypeException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

/**
 * Tests {@code DefaultParameterizedType}.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultParameterizedTypeTest.java 2 2009-02-02 22:28:39Z markhobson $
 * @see DefaultParameterizedType
 */
public class DefaultParameterizedTypeTest
{
	// tests ------------------------------------------------------------------
	
	@Test
	public void constructor()
	{
		ParameterizedType type = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		assertEquals(Map.class, type.getOwnerType());
		assertEquals(Map.Entry.class, type.getRawType());
		assertArrayEquals(new Type[] {String.class, Integer.class}, type.getActualTypeArguments());
	}
	
	@Test
	public void constructorWithNullOwnerType()
	{
		ParameterizedType type = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		
		assertNull(type.getOwnerType());
		assertEquals(List.class, type.getRawType());
		assertArrayEquals(new Type[] {String.class}, type.getActualTypeArguments());
	}
	
	@Test(expected = NullPointerException.class)
	public void constructorWithNullRawType()
	{
		try
		{
			new DefaultParameterizedType(null, null, new Type[] {String.class});
		}
		catch (NullPointerException exception)
		{
			assertEquals("rawType cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test
	public void constructorWithNullActualTypeArguments()
	{
		ParameterizedType type = new DefaultParameterizedType(null, String.class, null);
		
		assertNull(type.getOwnerType());
		assertEquals(String.class, type.getRawType());
		assertArrayEquals(new Type[0], type.getActualTypeArguments());
	}
	
	@Test(expected = MalformedParameterizedTypeException.class)
	public void constructorWithMismatchedActualTypeArguments()
	{
		new DefaultParameterizedType(null, List.class, null);
	}
	
	@Test
	public void hashCodeTest()
	{
		ParameterizedType type1 = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		ParameterizedType type2 = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		assertEquals(type1.hashCode(), type2.hashCode());
	}
	
	@Test
	public void hashCodeWithNullOwnerType()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		ParameterizedType type2 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		
		assertEquals(type1.hashCode(), type2.hashCode());
	}
	
	@Test
	public void hashCodeWithNullActualTypeArguments()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, String.class, null);
		ParameterizedType type2 = new DefaultParameterizedType(null, String.class, null);
		
		assertEquals(type1.hashCode(), type2.hashCode());
	}
	
	@Test
	public void equalsWhenEqual()
	{
		ParameterizedType type1 = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		ParameterizedType type2 = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		assertEquals(type1, type2);
	}
	
	@Test
	public void equalsWithDifferentClass()
	{
		ParameterizedType type = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		
		assertFalse(type.equals(new Object()));
	}
	
	@Test
	public void equalsWithNullOwnerType()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		ParameterizedType type2 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		
		assertEquals(type1, type2);
	}
	
	@Test
	public void equalsWithNullActualTypeArguments()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, String.class, null);
		ParameterizedType type2 = new DefaultParameterizedType(null, String.class, null);
		
		assertEquals(type1, type2);
	}
	
	@Test
	public void equalsWithUnequalOwnerType()
	{
		ParameterizedType type1 = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		ParameterizedType type2 = new DefaultParameterizedType(List.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		assertFalse(type1.equals(type2));
	}
	
	@Test
	public void equalsWithUnequalRawType()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		ParameterizedType type2 = new DefaultParameterizedType(null, Set.class, new Type[] {String.class});
		
		assertFalse(type1.equals(type2));
	}
	
	@Test
	public void equalsWithUnequalActualTypeArguments()
	{
		ParameterizedType type1 = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		ParameterizedType type2 = new DefaultParameterizedType(null, List.class, new Type[] {Integer.class});
		
		assertFalse(type1.equals(type2));
	}
	
	@Test
	public void toStringWithOwnerType()
	{
		ParameterizedType type = new DefaultParameterizedType(Map.class, Map.Entry.class,
			new Type[] {String.class, Integer.class});
		
		assertEquals("java.util.Map.java.util.Map$Entry<java.lang.String,java.lang.Integer>", type.toString());
	}
	
	@Test
	public void toStringWithRawType()
	{
		ParameterizedType type = new DefaultParameterizedType(null, String.class, null);
		
		assertEquals("java.lang.String", type.toString());
	}
	
	@Test
	public void toStringWithRawTypeAndActualTypeArgument()
	{
		ParameterizedType type = new DefaultParameterizedType(null, List.class, new Type[] {String.class});
		
		assertEquals("java.util.List<java.lang.String>", type.toString());
	}
	
	@Test
	public void toStringWithRawTypeAndActualTypeArguments()
	{
		ParameterizedType type = new DefaultParameterizedType(null, Map.class,
			new Type[] {String.class, Integer.class});
		
		assertEquals("java.util.Map<java.lang.String,java.lang.Integer>", type.toString());
	}
}
