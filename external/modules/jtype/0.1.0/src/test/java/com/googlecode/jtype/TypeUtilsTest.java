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
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Tests {@code TypeUtils}.
 * 
 * @author Mark Hobson
 * @version $Id: TypeUtilsTest.java 3 2009-02-03 21:40:20Z markhobson $
 * @see TypeUtils
 */
public class TypeUtilsTest
{
	// classes ----------------------------------------------------------------
	
	private static class IntegerArrayList extends ArrayList<Integer>
	{
		// simple subclass to fix generics
	}
	
	// fields -----------------------------------------------------------------
	
	private Set<String> valueOfImports;
	
	private GenericDeclaration declaration;
	
	// public methods ---------------------------------------------------------
	
	@Before
	public void setUp() throws NoSuchMethodException
	{
		valueOfImports = Collections.unmodifiableSet(new HashSet<String>(Arrays.asList(
			Object.class.getName(),
			Number.class.getName(),
			Integer.class.getName(),
			Collection.class.getName(),
			Set.class.getName(),
			List.class.getName(),
			HashSet.class.getName(),
			LinkedHashSet.class.getName(),
			ArrayList.class.getName()
		)));
		
		declaration = getClass().getConstructor();
	}
	
	// isAssignable tests -----------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void isAssignableWithNullSupertype()
	{
		TypeUtils.isAssignable(null, Integer.class);
	}
	
	@Test(expected = NullPointerException.class)
	public void isAssignableWithNullType()
	{
		TypeUtils.isAssignable(Integer.class, null);
	}
	
	/**
	 * Tests that classes are assignable to their direct superclasses.
	 * 
	 * {@literal Number <: Integer}
	 */
	@Test
	public void isAssignableWithDirectSuperclassFromClass()
	{
		assertTrue(TypeUtils.isAssignable(Number.class, Integer.class));
		assertFalse(TypeUtils.isAssignable(Integer.class, Number.class));
	}
	
	/**
	 * Tests that classes are assignable to their indirect superclasses.
	 * 
	 * {@literal Object <: Integer}
	 */
	@Test
	public void isAssignableWithIndirectSuperclassFromClass()
	{
		assertTrue(TypeUtils.isAssignable(Object.class, Integer.class));
		assertFalse(TypeUtils.isAssignable(Integer.class, Object.class));
	}
	
	/**
	 * Tests that parameterized types are assignable to their raw types.
	 * 
	 * {@literal List <: List<Integer>}
	 */
	@Test
	public void isAssignableWithClassFromParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(List.class, valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), List.class));
	}
	
	/**
	 * Tests that parameterized types are assignable if their raw types are directly assignable.
	 * 
	 * {@literal Collection<Integer> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithDirectlyAssignableParameterizedTypeRawTypes()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("Collection<Integer>"), valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("Collection<Integer>")));
	}
	
	/**
	 * Tests that parameterized types are assignable if their raw types are indirectly assignable.
	 * 
	 * {@literal Collection<Integer> <: ArrayList<Integer>}
	 */
	@Test
	public void isAssignableWithIndirectlyAssignableParameterizedTypeRawTypes()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("Collection<Integer>"), valueOf("ArrayList<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("ArrayList<Integer>"), valueOf("Collection<Integer>")));
	}
	
	/**
	 * Tests that parameterized types are not assignable if their raw types are not assignable.
	 * 
	 * {@literal List<Integer> !<: Set<Integer>}
	 */
	@Test
	public void isAssignableWithUnassignableParameterizedTypeRawTypes()
	{
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("Set<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("Set<Integer>"), valueOf("List<Integer>")));
	}
	
	/**
	 * Tests that parameterized types are not assignable even if their type arguments are assignable.
	 * 
	 * {@literal List<Number> !<: List<Integer>}
	 */
	@Test
	public void isAssignableWithAssignableParameterizedTypeArguments()
	{
		assertFalse(TypeUtils.isAssignable(valueOf("List<Number>"), valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("List<Number>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types.
	 * 
	 * {@literal List<?> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithWildcardParameterizedTypeFromParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<?>"), valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("List<?>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types on their upper bound.
	 * 
	 * {@literal List<? extends Number> <: List<Number>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? extends Number>"), valueOf("List<Number>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Number>"), valueOf("List<? extends Number>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types within their upper bound.
	 * 
	 * {@literal List<? extends Number> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromInBoundsParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? extends Number>"), valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("List<? extends Number>")));
	}
	
	/**
	 * Tests that parameterized type arguments are not assignable to wildcard types outside of their upper bound.
	 * 
	 * {@literal List<? extends Number> !<: List<Object>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromOutOfBoundsParameterizedType()
	{
		assertFalse(TypeUtils.isAssignable(valueOf("List<? extends Number>"), valueOf("List<Object>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Object>"), valueOf("List<? extends Number>")));
	}
	
	/**
	 * {@literal List<? extends Number> <: List<? extends Integer>}
	 */
	// TODO: fix
	@Ignore
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromInBoundsUpperBoundedWildcardParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? extends Number>"), valueOf("List<? extends Integer>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types on their lower bound.
	 * 
	 * {@literal List<? super Number> <: List<Number>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? super Number>"), valueOf("List<Number>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Number>"), valueOf("List<? super Number>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types within their lower bound.
	 * 
	 * {@literal List<? super Number> <: List<Object>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromInBoundsParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? super Number>"), valueOf("List<Object>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Object>"), valueOf("List<? super Number>")));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types outside of their lower bound.
	 * 
	 * {@literal List<? super Number> !<: List<Integer>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromOutOfBoundsParameterizedType()
	{
		assertFalse(TypeUtils.isAssignable(valueOf("List<? super Number>"), valueOf("List<Integer>")));
		assertFalse(TypeUtils.isAssignable(valueOf("List<Integer>"), valueOf("List<? super Number>")));
	}
	
	/**
	 * {@literal List<? super Integer> <: List<? super Number>}
	 */
	// TODO: fix
	@Ignore
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromInBoundsLowerBoundedWildcardParameterizedType()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<? super Integer>"), valueOf("List<? super Number>")));
	}
	
	/**
	 * Tests that classes are assignable to parameterized supertypes.
	 * 
	 * {@literal List<Integer> <: IntegerArrayList}
	 */
	@Test
	public void isAssignableWithParameterizedTypeFromClass()
	{
		assertTrue(TypeUtils.isAssignable(valueOf("List<Integer>"), IntegerArrayList.class));
		assertFalse(TypeUtils.isAssignable(IntegerArrayList.class, valueOf("List<Integer>")));
	}
	
	// isInstance tests -------------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void isInstanceWithNullType()
	{
		TypeUtils.isInstance(null, Integer.valueOf(123));
	}
	
	@Test
	public void isInstanceWithClass()
	{
		assertTrue(TypeUtils.isInstance(Integer.class, 123));
	}
	
	@Test
	public void isInstanceWithClassAndSubclass()
	{
		assertTrue(TypeUtils.isInstance(Number.class, 123));
	}
	
	@Test
	public void isInstanceWithClassAndSuperclass()
	{
		assertFalse(TypeUtils.isInstance(Number.class, new Object()));
	}
	
	@Test
	public void isInstanceWithClassAndDisjointClass()
	{
		assertFalse(TypeUtils.isInstance(Integer.class, 123L));
	}
	
	@Test
	public void isInstanceWithClassAndNull()
	{
		assertFalse(TypeUtils.isInstance(Integer.class, null));
	}
	
	@Test
	public void isInstanceWithClassArray()
	{
		assertTrue(TypeUtils.isInstance(Integer[].class, new Integer[0]));
	}
	
	@Test
	public void isInstanceWithClassArrayAndSubclassArray()
	{
		assertTrue(TypeUtils.isInstance(Number[].class, new Integer[0]));
	}
	
	@Test
	public void isInstanceWithClassArrayAndSuperclassArray()
	{
		assertFalse(TypeUtils.isInstance(Number[].class, new Object[0]));
	}
	
	@Test
	public void isInstanceWithClassArrayAndDisjointClass()
	{
		assertFalse(TypeUtils.isInstance(Integer[].class, new Long[0]));
	}
	
	@Test
	public void isInstanceWithGenericArrayType()
	{
		assertTrue(TypeUtils.isInstance(valueOf("List<Integer>[]"), new List[0]));
	}
	
	@Test
	public void isInstanceWithGenericArrayTypeAndSubclassComponentType()
	{
		assertTrue(TypeUtils.isInstance(valueOf("Collection<Integer>[]"), new List[0]));
	}
	
	@Test
	public void isInstanceWithGenericArrayTypeAndSuperclassComponentType()
	{
		assertFalse(TypeUtils.isInstance(valueOf("List<Integer>[]"), new Collection[0]));
	}
	
	@Test
	public void isInstanceWithGenericArrayTypeWithDisjointComponentType()
	{
		assertFalse(TypeUtils.isInstance(valueOf("List<Integer>[]"), new Set[0]));
	}
	
	@Test
	public void isInstanceWithParameterizedType()
	{
		assertTrue(TypeUtils.isInstance(valueOf("ArrayList<Integer>"), new ArrayList<Integer>()));
	}
	
	@Test
	public void isInstanceWithParameterizedTypeWithSubclassRawType()
	{
		assertTrue(TypeUtils.isInstance(valueOf("Collection<Integer>"), new ArrayList<Integer>()));
	}
	
	@Test
	public void isInstanceWithParameterizedTypeWithSuperclassRawType()
	{
		assertFalse(TypeUtils.isInstance(valueOf("LinkedHashSet<Integer>"), new HashSet<Integer>()));
	}
	
	@Test
	public void isInstanceWithParameterizedTypeWithDisjointRawType()
	{
		assertFalse(TypeUtils.isInstance(valueOf("List<Integer>"), new HashSet<Integer>()));
	}
	
	@Test
	public void isInstanceWithParameterizedTypeWithDisjointTypeArgument()
	{
		// support erasure
		assertTrue(TypeUtils.isInstance(valueOf("ArrayList<Integer>"), new ArrayList<Long>()));
	}
	
	// getErasedType tests ----------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void getErasedTypeWithNull()
	{
		TypeUtils.getErasedType(null);
	}
	
	@Test
	public void getErasedTypeWithClass()
	{
		assertEquals(Integer.class, TypeUtils.getErasedType(Integer.class));
	}
	
	@Test
	public void getErasedTypeWithClassArray()
	{
		assertEquals(Integer[].class, TypeUtils.getErasedType(Integer[].class));
	}
	
	@Test
	public void getErasedTypeWithTypeVariable()
	{
		assertEquals(Number.class, TypeUtils.getErasedType(Types.typeVariable(declaration, "T", Number.class,
			Comparable.class)));
	}
	
	@Test
	public void getErasedTypeWithGenericArrayType()
	{
		assertEquals(List[].class, TypeUtils.getErasedType(valueOf("List<Integer>[]")));
	}
	
	@Test
	public void getErasedTypeWithParameterizedType()
	{
		assertEquals(List.class, TypeUtils.getErasedType(valueOf("List<Integer>")));
	}
	
	@Test
	public void getErasedTypeWithWildcardType()
	{
		Type type = Types.unboundedWildcardType();
		
		assertEquals(type, TypeUtils.getErasedType(type));
	}
	
	// getRawType tests -------------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void getRawTypeWithNull()
	{
		TypeUtils.getRawType(null);
	}
	
	@Test
	public void getRawTypeWithClass()
	{
		assertEquals(Integer.class, TypeUtils.getRawType(Integer.class));
	}
	
	@Test
	public void getRawTypeWithGenericArrayType()
	{
		assertEquals(List[].class, TypeUtils.getRawType(valueOf("List<Integer>[]")));
	}
	
	@Test
	public void getRawTypeWithParameterizedType()
	{
		assertEquals(List.class, TypeUtils.getRawType(valueOf("List<Integer>")));
	}
	
	// isArray tests ----------------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void isArrayWithNull()
	{
		TypeUtils.isArray(null);
	}
	
	@Test
	public void isArrayWithClass()
	{
		assertFalse(TypeUtils.isArray(Integer.class));
	}
	
	@Test
	public void isArrayWithClassArray()
	{
		assertTrue(TypeUtils.isArray(Integer[].class));
	}
	
	@Test
	public void isArrayWithTypeVariable()
	{
		assertFalse(TypeUtils.isArray(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void isArrayWithGenericArrayType()
	{
		assertTrue(TypeUtils.isArray(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void isArrayWithParameterizedType()
	{
		assertFalse(TypeUtils.isArray(Types.parameterizedType(List.class, Integer.class)));
	}
	
	@Test
	public void isArrayWithWildcardType()
	{
		assertFalse(TypeUtils.isArray(Types.unboundedWildcardType()));
	}
	
	// getComponentType tests -------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void getComponentTypeWithNull()
	{
		TypeUtils.getComponentType(null);
	}
	
	@Test
	public void getComponentTypeWithClass()
	{
		assertNull(TypeUtils.getComponentType(Integer.class));
	}
	
	@Test
	public void getComponentTypeWithClassArray()
	{
		assertEquals(Integer.class, TypeUtils.getComponentType(Integer[].class));
	}
	
	@Test
	public void getComponentTypeWithTypeVariable()
	{
		assertNull(TypeUtils.getComponentType(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void getComponentTypeWithGenericArrayType()
	{
		assertEquals(Integer.class, TypeUtils.getComponentType(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void getComponentTypeWithParameterizedType()
	{
		assertNull(TypeUtils.getComponentType(Types.parameterizedType(List.class, Integer.class)));
	}
	
	@Test
	public void getComponentTypeWithWildcardType()
	{
		assertNull(TypeUtils.getComponentType(Types.unboundedWildcardType()));
	}
	
	// getArrayType tests -----------------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void getArrayTypeWithNull()
	{
		TypeUtils.getArrayType(null);
	}
	
	@Test
	public void getArrayTypeWithClass()
	{
		assertEquals(Integer[].class, TypeUtils.getArrayType(Integer.class));
	}
	
	@Test
	public void getArrayTypeWithTypeVariable()
	{
		assertEquals(Types.genericArrayType(Types.typeVariable(declaration, "T")),
			TypeUtils.getArrayType(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void getArrayTypeWithGenericArrayType()
	{
		assertEquals(Types.genericArrayType(Types.genericArrayType(Integer.class)),
			TypeUtils.getArrayType(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void getArrayTypeWithParameterizedType()
	{
		assertEquals(Types.genericArrayType(Types.parameterizedType(List.class, String.class)),
			TypeUtils.getArrayType(Types.parameterizedType(List.class, String.class)));
	}
	
	@Test
	public void getArrayTypeWithWildcardType()
	{
		assertEquals(Types.genericArrayType(Types.unboundedWildcardType()),
			TypeUtils.getArrayType(Types.unboundedWildcardType()));
	}
	
	// isSimpleParameterizedType tests ----------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void isSimpleParameterizedTypeWithNullType()
	{
		TypeUtils.isSimpleParameterizedType(null, List.class);
	}
	
	@Test(expected = NullPointerException.class)
	public void isSimpleParameterizedTypeWithNullRawType()
	{
		TypeUtils.isSimpleParameterizedType(List.class, null);
	}
	
	@Test
	public void isSimpleParameterizedTypeWithSameRawType()
	{
		assertTrue(TypeUtils.isSimpleParameterizedType(valueOf("List<Integer>"), List.class));
	}
	
	@Test
	public void isSimpleParameterizedTypeWithSuperRawType()
	{
		assertTrue(TypeUtils.isSimpleParameterizedType(valueOf("List<Integer>"), Collection.class));
	}
	
	@Test
	public void isSimpleParameterizedTypeWithUnassignableRawType()
	{
		assertFalse(TypeUtils.isSimpleParameterizedType(valueOf("List<Integer>"), Number.class));
	}
	
	@Test
	public void isSimpleParameterizedTypeWithClass()
	{
		assertFalse(TypeUtils.isSimpleParameterizedType(List.class, List.class));
	}
	
	@Test
	public void isSimpleParameterizedTypeWithParameterizedTypeArgument()
	{
		assertTrue(TypeUtils.isSimpleParameterizedType(valueOf("List<List<Integer>>"), List.class));
	}
	
	// getActualTypeArgument tests --------------------------------------------
	
	@Test(expected = NullPointerException.class)
	public void getActualTypeArgumentWithNull()
	{
		TypeUtils.getActualTypeArgument(null);
	}
	
	@Test
	public void getActualTypeArgument()
	{
		assertEquals(Integer.class, TypeUtils.getActualTypeArgument(valueOf("List<Integer>")));
	}
	
	// toString tests ---------------------------------------------------------
	
	@Test
	public void toStringWithClass()
	{
		assertEquals("java.lang.Integer", TypeUtils.toString(Integer.class));
	}
	
	@Test
	public void toStringWithArrayClass()
	{
		assertEquals("java.lang.Integer[]", TypeUtils.toString(Integer[].class));
	}
	
	@Test
	public void toStringWithUnboundedTypeVariable()
	{
		assertEquals("T", TypeUtils.toString(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void toStringWithBoundedTypeVariable()
	{
		assertEquals("T extends java.lang.Number & java.lang.Comparable",
			TypeUtils.toString(Types.typeVariable(declaration, "T", Number.class, Comparable.class)));
	}
	
	// TODO: fix
	@Test
	@Ignore
	public void toStringWithRecursiveTypeVariable()
	{
		// TODO: create recursive type variable
		TypeVariable<GenericDeclaration> typeVariable = Types.typeVariable(declaration, "T",
			Types.parameterizedType(Comparable.class, /*typeVariable*/ Object.class));
		
		assertEquals("T extends java.lang.Comparable<T>", TypeUtils.toString(typeVariable));
	}
	
	@Test
	public void toStringWithGenericArrayType()
	{
		assertEquals("java.lang.Integer[]", TypeUtils.toString(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void toStringWithParameterizedType()
	{
		assertEquals("java.util.List<java.lang.Integer>",
			TypeUtils.toString(Types.parameterizedType(List.class, Integer.class)));
	}
	
	@Test
	public void toStringWithParameterizedTypeByTypeVariable()
	{
		assertEquals("java.util.List<T>",
			TypeUtils.toString(Types.parameterizedType(List.class, Types.typeVariable(declaration, "T"))));
	}
	
	// TODO: fix
	@Test
	@Ignore
	public void toStringWithParameterizedTypeByTypeVariableWithBounds()
	{
		assertEquals("java.util.List<T>",
			TypeUtils.toString(Types.parameterizedType(List.class,
				Types.typeVariable(declaration, "T", Number.class))));
	}
	
	@Test
	public void toStringWithUnboundedWildcardType()
	{
		assertEquals("?", TypeUtils.toString(Types.unboundedWildcardType()));
	}
	
	@Test
	public void toStringWithUpperBoundedWildcardType()
	{
		assertEquals("? extends java.lang.Number", TypeUtils.toString(Types.upperBoundedWildcardType(Number.class)));
	}
	
	@Test
	public void toStringWithLowerBoundedWildcardType()
	{
		assertEquals("? super java.lang.Integer", TypeUtils.toString(Types.lowerBoundedWildcardType(Integer.class)));
	}
	
	@Test
	public void toStringWithNull()
	{
		assertEquals("null", TypeUtils.toString(null));
	}
	
	// toUnqualifiedString tests ----------------------------------------------
	
	@Test
	public void toUnqualifiedStringWithClass()
	{
		assertEquals("Integer", TypeUtils.toUnqualifiedString(Integer.class));
	}
	
	@Test
	public void toUnqualifiedStringWithArrayClass()
	{
		assertEquals("Integer[]", TypeUtils.toUnqualifiedString(Integer[].class));
	}
	
	@Test
	public void toUnqualifiedStringWithUnboundedTypeVariable()
	{
		assertEquals("T", TypeUtils.toUnqualifiedString(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void toUnqualifiedStringWithBoundedTypeVariable()
	{
		TypeVariable<?> type = Types.typeVariable(declaration, "T", Number.class, Comparable.class);
		
		assertEquals("T extends Number & Comparable", TypeUtils.toUnqualifiedString(type));
	}
	
	@Test
	public void toUnqualifiedStringWithGenericArrayType()
	{
		assertEquals("Integer[]", TypeUtils.toUnqualifiedString(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void toUnqualifiedStringWithParameterizedType()
	{
		ParameterizedType type = Types.parameterizedType(List.class, Integer.class);
		
		assertEquals("List<Integer>", TypeUtils.toUnqualifiedString(type));
	}
	
	@Test
	public void toUnqualifiedStringWithUnboundedWildcardType()
	{
		assertEquals("?", TypeUtils.toUnqualifiedString(Types.unboundedWildcardType()));
	}
	
	@Test
	public void toUnqualifiedStringWithUpperBoundedWildcardType()
	{
		WildcardType type = Types.upperBoundedWildcardType(Number.class);
		
		assertEquals("? extends Number", TypeUtils.toUnqualifiedString(type));
	}
	
	@Test
	public void toUnqualifiedStringWithLowerBoundedWildcardType()
	{
		WildcardType type = Types.lowerBoundedWildcardType(Integer.class);
		
		assertEquals("? super Integer", TypeUtils.toUnqualifiedString(type));
	}
	
	@Test
	public void toUnqualifiedStringWithNull()
	{
		assertEquals("null", TypeUtils.toUnqualifiedString(null));
	}
	
	// private methods --------------------------------------------------------
	
	private Type valueOf(String typeName)
	{
		return Types.valueOf(typeName, valueOfImports);
	}
}
