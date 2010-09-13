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

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;

import org.junit.Test;

/**
 * Tests {@code DefaultWildcardType}.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultWildcardTypeTest.java 2 2009-02-02 22:28:39Z markhobson $
 * @see DefaultWildcardType
 */
public class DefaultWildcardTypeTest
{
	// tests ------------------------------------------------------------------
	
	@Test
	public void constructorWithNullUpperBounds()
	{
		WildcardType wildcardType = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertArrayEquals(new Type[] {Object.class}, wildcardType.getUpperBounds());
		assertArrayEquals(new Type[] {Integer.class}, wildcardType.getLowerBounds());
	}
	
	@Test
	public void constructorWithEmptyUpperBounds()
	{
		WildcardType wildcardType = new DefaultWildcardType(new Type[0], new Type[] {Integer.class});
		
		assertArrayEquals(new Type[] {Object.class}, wildcardType.getUpperBounds());
		assertArrayEquals(new Type[] {Integer.class}, wildcardType.getLowerBounds());
	}
	
	@Test
	public void constructorWithNullLowerBounds()
	{
		WildcardType wildcardType = new DefaultWildcardType(new Type[] {Number.class}, null);
		
		assertArrayEquals(new Type[] {Number.class}, wildcardType.getUpperBounds());
		assertArrayEquals(new Type[0], wildcardType.getLowerBounds());
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void constructorWithUpperAndLowerBounds()
	{
		try
		{
			new DefaultWildcardType(new Type[] {Number.class}, new Type[] {Integer.class});
		}
		catch (IllegalArgumentException exception)
		{
			assertEquals("Wildcard type cannot have both upper and lower bounds", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test
	public void hashCodeTest()
	{
		WildcardType wildcardType1 = new DefaultWildcardType(null, new Type[] {Integer.class});
		WildcardType wildcardType2 = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertEquals(wildcardType1.hashCode(), wildcardType2.hashCode());
	}
	
	@Test
	public void equalsWhenEqual()
	{
		WildcardType wildcardType1 = new DefaultWildcardType(null, new Type[] {Integer.class});
		WildcardType wildcardType2 = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertEquals(wildcardType1, wildcardType2);
	}
	
	@Test
	public void equalsWithDifferentClass()
	{
		WildcardType wildcardType = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertFalse(wildcardType.equals(new Object()));
	}
	
	@Test
	public void equalsWithDifferentBounds()
	{
		WildcardType wildcardType1 = new DefaultWildcardType(new Type[] {Number.class}, null);
		WildcardType wildcardType2 = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertFalse(wildcardType1.equals(wildcardType2));
	}
	
	@Test
	public void equalsWithDifferentUpperBounds()
	{
		WildcardType wildcardType1 = new DefaultWildcardType(new Type[] {Number.class}, null);
		WildcardType wildcardType2 = new DefaultWildcardType(new Type[] {Integer.class}, null);
		
		assertFalse(wildcardType1.equals(wildcardType2));
	}
	
	@Test
	public void equalsWithDifferentLowerBounds()
	{
		WildcardType wildcardType1 = new DefaultWildcardType(null, new Type[] {Number.class});
		WildcardType wildcardType2 = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertFalse(wildcardType1.equals(wildcardType2));
	}
	
	@Test
	public void toStringWithNoBounds()
	{
		WildcardType wildcardType = new DefaultWildcardType(null, null);
		
		assertEquals("?", wildcardType.toString());
	}
	
	@Test
	public void toStringWithSingleUpperBound()
	{
		WildcardType wildcardType = new DefaultWildcardType(new Type[] {Number.class}, null);
		
		assertEquals("? extends java.lang.Number", wildcardType.toString());
	}
	
	@Test
	public void toStringWithMultipleUpperBounds()
	{
		WildcardType wildcardType = new DefaultWildcardType(new Type[] {Number.class, Comparable.class}, null);
		
		assertEquals("? extends java.lang.Number & java.lang.Comparable", wildcardType.toString());
	}
	
	@Test
	public void toStringWithSingleLowerBound()
	{
		WildcardType wildcardType = new DefaultWildcardType(null, new Type[] {Integer.class});
		
		assertEquals("? super java.lang.Integer", wildcardType.toString());
	}
	
	@Test
	public void toStringWithMultipleLowerBound()
	{
		WildcardType wildcardType = new DefaultWildcardType(null, new Type[] {Integer.class, Number.class});
		
		assertEquals("? super java.lang.Integer & java.lang.Number", wildcardType.toString());
	}
}
