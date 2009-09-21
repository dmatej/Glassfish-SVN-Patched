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

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests {@code DefaultTypeVariable}.
 * 
 * @author Mark Hobson
 * @version $Id: DefaultTypeVariableTest.java 2 2009-02-02 22:28:39Z markhobson $
 * @see DefaultTypeVariable
 */
public class DefaultTypeVariableTest
{
	// fields -----------------------------------------------------------------
	
	private Constructor<?> constructor;
	
	// public methods ---------------------------------------------------------
	
	@Before
	public void setUp() throws NoSuchMethodException
	{
		constructor = getClass().getConstructor();
	}
	
	// tests ------------------------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void constructorWithNullDeclaration()
	{
		try
		{
			new DefaultTypeVariable<Constructor<?>>(null, "T", new Type[] {Number.class});
		}
		catch (NullPointerException exception)
		{
			assertEquals("declaration cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test(expected = NullPointerException.class)
	public void constructorWithNullName()
	{
		try
		{
			new DefaultTypeVariable<Constructor<?>>(constructor, null, new Type[] {Number.class});
		}
		catch (NullPointerException exception)
		{
			assertEquals("name cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test
	public void constructorWithBounds()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class});
		
		assertEquals(constructor, typeVariable.getGenericDeclaration());
		assertEquals("T", typeVariable.getName());
		assertArrayEquals(new Type[] {Number.class}, typeVariable.getBounds());
	}
	
	@Test
	public void constructorWithNullBounds()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T", null);
		
		assertEquals(constructor, typeVariable.getGenericDeclaration());
		assertEquals("T", typeVariable.getName());
		assertArrayEquals(new Type[] {Object.class}, typeVariable.getBounds());
	}
	
	@Test
	public void constructorWithEmptyBounds()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[0]);
		
		assertEquals(constructor, typeVariable.getGenericDeclaration());
		assertEquals("T", typeVariable.getName());
		assertArrayEquals(new Type[] {Object.class}, typeVariable.getBounds());
	}
	
	@Test
	public void hashCodeTest()
	{
		TypeVariable<Constructor<?>> typeVariable1 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class, Comparable.class});
		
		TypeVariable<Constructor<?>> typeVariable2 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class, Comparable.class});
		
		assertEquals(typeVariable1.hashCode(), typeVariable2.hashCode());
	}
	
	@Test
	public void equalsWhenEqual()
	{
		TypeVariable<Constructor<?>> typeVariable1 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class, Comparable.class});
		
		TypeVariable<Constructor<?>> typeVariable2 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class, Comparable.class});
		
		assertEquals(typeVariable1, typeVariable2);
	}
	
	@Test
	public void equalsWithDifferentClass()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T", null);
		
		assertFalse(typeVariable.equals(new Object()));
	}
	
	@Test
	public void equalsWithDifferentDeclarations() throws NoSuchMethodException
	{
		TypeVariable<Constructor<?>> typeVariable1 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class});
		
		Method method = getClass().getDeclaredMethod("equalsWithDifferentDeclarations");
		
		TypeVariable<Method> typeVariable2 = new DefaultTypeVariable<Method>(method, "T", new Type[] {Number.class});
		
		assertFalse(typeVariable1.equals(typeVariable2));
	}
	
	@Test
	public void equalsWithDifferentNames()
	{
		TypeVariable<Constructor<?>> typeVariable1 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class});
		
		TypeVariable<Constructor<?>> typeVariable2 = new DefaultTypeVariable<Constructor<?>>(constructor, "U",
			new Type[] {Number.class});
		
		assertFalse(typeVariable1.equals(typeVariable2));
	}
	
	@Test
	public void equalsWithDifferentBounds()
	{
		TypeVariable<Constructor<?>> typeVariable1 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class});
		
		TypeVariable<Constructor<?>> typeVariable2 = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Integer.class});
		
		assertFalse(typeVariable1.equals(typeVariable2));
	}
	
	@Test
	public void toStringWithNoBounds()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T", null);
		
		assertEquals("T", typeVariable.toString());
	}
	
	@Test
	public void toStringWithSingleBound()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class});
		
		assertEquals("T extends java.lang.Number", typeVariable.toString());
	}
	
	@Test
	public void toStringWithMultipleBounds()
	{
		TypeVariable<Constructor<?>> typeVariable = new DefaultTypeVariable<Constructor<?>>(constructor, "T",
			new Type[] {Number.class, Comparable.class});
		
		assertEquals("T extends java.lang.Number & java.lang.Comparable", typeVariable.toString());
	}
}
