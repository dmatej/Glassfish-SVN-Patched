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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.lang.reflect.Constructor;
import java.lang.reflect.GenericArrayType;
import java.lang.reflect.MalformedParameterizedTypeException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jmock.Expectations;
import org.jmock.Mockery;
import org.jmock.integration.junit4.JMock;
import org.jmock.integration.junit4.JUnit4Mockery;
import org.jmock.lib.legacy.ClassImposteriser;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Tests {@code Types}.
 * 
 * @author Mark Hobson
 * @version $Id: TypesTest.java 37 2009-07-24 11:27:39Z markhobson $
 * @see Types
 */
@RunWith(JMock.class)
public class TypesTest
{
	// fields -----------------------------------------------------------------
	
	private Mockery context;
	
	@SuppressWarnings("unused")
	private Map<String, Integer> stringIntegerMap;
	
	private Type stringIntegerMapType;
	
	@SuppressWarnings("unused")
	private List<?> unboundedWildcardList;
	
	private Type unboundedWildcardType;
	
	@SuppressWarnings("unused")
	private List<? extends Number> numberUpperBoundedWildcardList;
	
	private Type numberUpperBoundedWildcardType;
	
	@SuppressWarnings("unused")
	private List<? super Integer> integerLowerBoundedWildcardList;
	
	private Type integerLowerBoundedWildcardType;
	
	// public methods ---------------------------------------------------------
	
	@Before
	public void setUp() throws NoSuchFieldException
	{
		context = new JUnit4Mockery();
		context.setImposteriser(ClassImposteriser.INSTANCE);
		
		stringIntegerMapType = getFieldType("stringIntegerMap");
		unboundedWildcardType = getFieldActualTypeArgument("unboundedWildcardList");
		numberUpperBoundedWildcardType = getFieldActualTypeArgument("numberUpperBoundedWildcardList");
		integerLowerBoundedWildcardType = getFieldActualTypeArgument("integerLowerBoundedWildcardList");
	}
	
	// typeVariable tests -----------------------------------------------------
	
	@Test
	public void typeVariableWithNoBounds() throws NoSuchMethodException
	{
		Constructor<TypesTest> constructor = TypesTest.class.getConstructor();
		
		TypeVariable<Constructor<TypesTest>> typeVariable = Types.typeVariable(constructor, "T");
		
		assertNotNull(typeVariable);
		assertEquals(constructor, typeVariable.getGenericDeclaration());
		assertEquals("T", typeVariable.getName());
		assertArrayEquals(new Type[] {Object.class}, typeVariable.getBounds());
	}
	
	@Test
	public void typeVariableWithBounds() throws NoSuchMethodException
	{
		Constructor<TypesTest> constructor = TypesTest.class.getConstructor();
		
		TypeVariable<Constructor<TypesTest>> typeVariable = Types.typeVariable(constructor, "T", Number.class,
			Comparable.class);
		
		assertNotNull(typeVariable);
		assertEquals(constructor, typeVariable.getGenericDeclaration());
		assertEquals("T", typeVariable.getName());
		assertArrayEquals(new Type[] {Number.class, Comparable.class}, typeVariable.getBounds());
	}
	
	// genericArrayType tests -------------------------------------------------
	
	@Test
	public void genericArrayType()
	{
		GenericArrayType type = Types.genericArrayType(Integer.class);
		
		assertNotNull(type);
		assertEquals(Integer.class, type.getGenericComponentType());
	}
	
	@Test(expected = NullPointerException.class)
	public void genericArrayTypeWithNull()
	{
		try
		{
			Types.genericArrayType(null);
		}
		catch (NullPointerException exception)
		{
			assertEquals("componentType cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	// parameterizedType tests ------------------------------------------------
	
	@Test
	public void parameterizedType()
	{
		ParameterizedType type = Types.parameterizedType(Map.class, new Type[] {String.class, Integer.class});
		
		assertNotNull(type);
		assertEquals(Map.class, type.getRawType());
		assertNull(type.getOwnerType());
		assertArrayEquals(new Type[] {String.class, Integer.class}, type.getActualTypeArguments());
		
		assertEquals(type, stringIntegerMapType);
		assertEquals(stringIntegerMapType, type);
	}
	
	@Test(expected = NullPointerException.class)
	public void parameterizedTypeWithNullRawType()
	{
		try
		{
			Types.parameterizedType(null, String.class, Integer.class);
		}
		catch (NullPointerException exception)
		{
			assertEquals("rawType cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test
	public void parameterizedTypeWithNoActualTypeArguments()
	{
		ParameterizedType type = Types.parameterizedType(Integer.class);
		
		assertNotNull(type);
		assertEquals(Integer.class, type.getRawType());
		assertNull(type.getOwnerType());
		assertArrayEquals(new Type[0], type.getActualTypeArguments());
	}
	
	@Test(expected = MalformedParameterizedTypeException.class)
	public void parameterizedTypeWithMismatchedActualTypeArguments()
	{
		Types.parameterizedType(Map.class, String.class);
	}
	
	// unboundedWildcardType tests --------------------------------------------
	
	@Test
	public void unboundedWildcardType()
	{
		WildcardType type = Types.unboundedWildcardType();
		
		assertNotNull(type);
		assertArrayEquals(new Type[] {Object.class}, type.getUpperBounds());
		assertArrayEquals(new Type[0], type.getLowerBounds());
		
		assertEquals(type, unboundedWildcardType);
		assertEquals(unboundedWildcardType, type);
	}
	
	// upperBoundedWildcardType tests -----------------------------------------
	
	@Test
	public void upperBoundedWildcardType()
	{
		WildcardType type = Types.upperBoundedWildcardType(Number.class);
		
		assertNotNull(type);
		assertArrayEquals(new Type[] {Number.class}, type.getUpperBounds());
		assertArrayEquals(new Type[0], type.getLowerBounds());
		
		assertEquals(type, numberUpperBoundedWildcardType);
		assertEquals(numberUpperBoundedWildcardType, type);
	}
	
	@Test(expected = NullPointerException.class)
	public void upperBoundedWildcardTypeWithNullUpperBound()
	{
		try
		{
			Types.upperBoundedWildcardType(null);
		}
		catch (NullPointerException exception)
		{
			assertEquals("upperBound cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	// lowerBoundedWildcardType tests -----------------------------------------
	
	@Test
	public void lowerBoundedWildcardType()
	{
		WildcardType type = Types.lowerBoundedWildcardType(Integer.class);
		
		assertNotNull(type);
		assertArrayEquals(new Type[] {Object.class}, type.getUpperBounds());
		assertArrayEquals(new Type[] {Integer.class}, type.getLowerBounds());
		
		assertEquals(type, integerLowerBoundedWildcardType);
		assertEquals(integerLowerBoundedWildcardType, type);
	}
	
	@Test(expected = NullPointerException.class)
	public void lowerBoundedWildcardTypeWithNullLowerBound()
	{
		try
		{
			Types.lowerBoundedWildcardType(null);
		}
		catch (NullPointerException exception)
		{
			assertEquals("lowerBound cannot be null", exception.getMessage());
			
			throw exception;
		}
	}
	
	// valueOf tests ----------------------------------------------------------
	
	@Test
	public void valueOfWithClass()
	{
		assertEquals(Integer.class, Types.valueOf("java.lang.Integer"));
	}
	
	@Test
	public void valueOfWithClassAndWhitespace()
	{
		assertEquals(Integer.class, Types.valueOf(" java.lang.Integer "));
	}
	
	@Test
	public void valueOfWithClassAndCustomClassLoader() throws ClassNotFoundException
	{
		ClassLoader oldClassLoader = Thread.currentThread().getContextClassLoader();
		
		final ClassLoader newClassLoader = context.mock(ClassLoader.class);
		
		context.checking(new Expectations() {{
			one(newClassLoader).loadClass("java.lang.Integer"); will(returnValue(Integer.class));
		}});

		try
		{
			Thread.currentThread().setContextClassLoader(newClassLoader);
			
			assertEquals(Integer.class, Types.valueOf("java.lang.Integer"));
		}
		finally
		{
			Thread.currentThread().setContextClassLoader(oldClassLoader);
		}
	}
	
	@Test
	public void valueOfWithArray()
	{
		assertEquals(Integer[].class, Types.valueOf("java.lang.Integer[]"));
	}
	
	@Test
	public void valueOfWithArrayAndWhitespace()
	{
		assertEquals(Integer[].class, Types.valueOf(" java.lang.Integer [ ] "));
	}
	
	@Test
	public void valueOfWithGenericArrayType()
	{
		assertEquals(Types.genericArrayType(Types.parameterizedType(List.class, Integer.class)),
			Types.valueOf("java.util.List<java.lang.Integer>[]"));
	}
	
	@Test
	public void valueOfWithSingleArgumentParameterizedType()
	{
		assertEquals(Types.parameterizedType(List.class, Integer.class),
			Types.valueOf("java.util.List<java.lang.Integer>"));
	}
	
	@Test
	public void valueOfWithSingleArgumentParameterizedTypeAndWhitespace()
	{
		assertEquals(Types.parameterizedType(List.class, Integer.class),
			Types.valueOf(" java.util.List < java.lang.Integer > "));
	}
	
	@Test
	public void valueOfWithSingleUnboundedWildcardParameterizedType()
	{
		assertEquals(Types.parameterizedType(List.class, Types.unboundedWildcardType()),
			Types.valueOf("java.util.List<?>"));
	}
	
	@Test
	public void valueOfWithMultipleArgumentParameterizedType()
	{
		assertEquals(Types.parameterizedType(Map.class, String.class, Integer.class),
			Types.valueOf("java.util.Map<java.lang.String,java.lang.Integer>"));
	}
	
	@Test
	public void valueOfWithMultipleArgumentParameterizedTypeAndWhitespace()
	{
		assertEquals(Types.parameterizedType(Map.class, String.class, Integer.class),
			Types.valueOf(" java.util.Map < java.lang.String , java.lang.Integer > "));
	}
	
	@Test
	public void valueOfWithMultipleUnboundedWildcardParameterizedType()
	{
		assertEquals(Types.parameterizedType(Map.class, Types.unboundedWildcardType(), Types.unboundedWildcardType()),
			Types.valueOf("java.util.Map<?,?>"));
	}
	
	@Test(expected = MalformedParameterizedTypeException.class)
	public void valueOfWithMismatchedArgumentParameterizedType()
	{
		Types.valueOf("java.util.Map<java.lang.String>");
	}
	
	@Test
	public void valueOfWithMultiParameterizedType()
	{
		assertEquals(Types.parameterizedType(List.class, Types.parameterizedType(List.class, Integer.class)),
			Types.valueOf("java.util.List<java.util.List<java.lang.Integer>>"));
	}
	
	@Test
	public void valueOfWithMultiAndMultipleArgumentParameterizedType()
	{
		Type expected = Types.parameterizedType(Map.class, Types.parameterizedType(List.class, String.class),
			Types.parameterizedType(List.class, Integer.class));
		
		assertEquals(expected,
			Types.valueOf("java.util.Map<java.util.List<java.lang.String>,java.util.List<java.lang.Integer>>"));
	}
	
	@Test
	public void valueOfWithUnboundedWildcardType()
	{
		assertEquals(Types.unboundedWildcardType(), Types.valueOf("?"));
	}
	
	@Test
	public void valueOfWithUnboundedWildcardTypeAndWhitespace()
	{
		assertEquals(Types.unboundedWildcardType(), Types.valueOf(" ? "));
	}
	
	@Test
	public void valueOfWithUpperBoundedWildcardType()
	{
		assertEquals(Types.upperBoundedWildcardType(Number.class), Types.valueOf("? extends java.lang.Number"));
	}
	
	@Test
	public void valueOfWithUpperBoundedWildcardTypeAndWhitespace()
	{
		assertEquals(Types.upperBoundedWildcardType(Number.class), Types.valueOf(" ?  extends  java.lang.Number "));
	}
	
	@Test
	public void valueOfWithLowerBoundedWildcardType()
	{
		assertEquals(Types.lowerBoundedWildcardType(Integer.class), Types.valueOf("? super java.lang.Integer"));
	}
	
	@Test
	public void valueOfWithLowerBoundedWildcardTypeAndWhitespace()
	{
		assertEquals(Types.lowerBoundedWildcardType(Integer.class), Types.valueOf(" ?  super  java.lang.Integer "));
	}
	
	@Test
	public void valueOfWithClassAndImportContext()
	{
		Set<String> importContext = Collections.singleton(Integer.class.getName());
		
		assertEquals(Integer.class, Types.valueOf("Integer", importContext));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void valueOfWithClassAndMissingImportContext()
	{
		try
		{
			Types.valueOf("Integer");
		}
		catch (IllegalArgumentException exception)
		{
			assertEquals("Class not found: Integer", exception.getMessage());
			
			throw exception;
		}
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void valueOfWithClassAndInvalidImportContext()
	{
		Set<String> importContext = new LinkedHashSet<String>();
		importContext.add(Integer.class.getName());
		importContext.add("another.Integer");
		
		try
		{
			Types.valueOf("Integer", importContext);
		}
		catch (IllegalArgumentException exception)
		{
			assertEquals("Duplicate imports: java.lang.Integer and another.Integer", exception.getMessage());
			
			throw exception;
		}
	}

	// TODO: fix
	@Ignore
	@Test
	public void valueOfWithMemberClassAndImportContext()
	{
		Set<String> importContext = Collections.singleton(Map.class.getName());
		
		assertEquals(Map.Entry.class, Types.valueOf("Map.Entry", importContext));
	}
	
	@Test
	public void valueOfWithMemberClassAndMemberImportContext()
	{
		Set<String> importContext = Collections.singleton(Map.Entry.class.getName());
		
		assertEquals(Map.Entry.class, Types.valueOf("Entry", importContext));
	}
	
	@Test
	public void valueOfWithParameterizedTypeAndImportContext()
	{
		Set<String> importContext = new HashSet<String>();
		importContext.add(List.class.getName());
		importContext.add(Integer.class.getName());
		
		assertEquals(Types.parameterizedType(List.class, Integer.class), Types.valueOf("List<Integer>", importContext));
	}
	
	@Test
	public void valueOfWithWilcardTypeAndImportContext()
	{
		Set<String> importContext = Collections.singleton(Number.class.getName());
		
		assertEquals(Types.upperBoundedWildcardType(Number.class), Types.valueOf("? extends Number", importContext));
	}
	
	// private methods --------------------------------------------------------
	
	private Type getFieldType(String name) throws NoSuchFieldException
	{
		return getClass().getDeclaredField(name).getGenericType();
	}
	
	private Type getFieldActualTypeArgument(String name) throws NoSuchFieldException
	{
		return ((ParameterizedType) getFieldType(name)).getActualTypeArguments()[0];
	}
}
