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
import static org.junit.Assert.assertTrue;

import java.io.Serializable;
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.RandomAccess;
import java.util.Set;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Tests {@code TypeUtils}.
 * 
 * @author Mark Hobson
 * @version $Id: TypeUtilsTest.java 48 2009-10-02 16:57:38Z markhobson $
 * @see TypeUtils
 */
public class TypeUtilsTest
{
	// classes ----------------------------------------------------------------
	
	private static class IntegerArrayList extends ArrayList<Integer>
	{
		// simple subclass to fix generics
	}
	
	private static class IntegerKeyHashMap<V> extends HashMap<Integer, V>
	{
		// simple subclass to fix generics
	}
	
	private static class StringsByIntegerHashMap extends IntegerKeyHashMap<String>
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
			Map.class.getName(),
			HashSet.class.getName(),
			LinkedHashSet.class.getName(),
			AbstractList.class.getName(),
			ArrayList.class.getName()
		)));
		
		declaration = getClass().getConstructor();
	}
	
	// isAssignable tests -----------------------------------------------------
	
	// JLS 4.10.1 Subtyping among Primitive Types
	
	@Test
	public void isAssignableWithPrimitiveDouble()
	{
		assertAssignable(Double.TYPE, Double.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Float.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Long.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Integer.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Character.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Short.TYPE);
		assertAsymmetricallyAssignable(Double.TYPE, Byte.TYPE);
	}
	
	@Test
	public void isAssignableWithPrimitiveFloat()
	{
		assertAssignable(Float.TYPE, Float.TYPE);
		assertAsymmetricallyAssignable(Float.TYPE, Long.TYPE);
		assertAsymmetricallyAssignable(Float.TYPE, Integer.TYPE);
		assertAsymmetricallyAssignable(Float.TYPE, Character.TYPE);
		assertAsymmetricallyAssignable(Float.TYPE, Short.TYPE);
		assertAsymmetricallyAssignable(Float.TYPE, Byte.TYPE);
	}
	
	@Test
	public void isAssignableWithPrimitiveLong()
	{
		assertAssignable(Long.TYPE, Long.TYPE);
		assertAsymmetricallyAssignable(Long.TYPE, Integer.TYPE);
		assertAsymmetricallyAssignable(Long.TYPE, Character.TYPE);
		assertAsymmetricallyAssignable(Long.TYPE, Short.TYPE);
		assertAsymmetricallyAssignable(Long.TYPE, Byte.TYPE);
	}
	
	@Test
	public void isAssignableWithPrimitiveInt()
	{
		assertAssignable(Integer.TYPE, Integer.TYPE);
		assertAsymmetricallyAssignable(Integer.TYPE, Character.TYPE);
		assertAsymmetricallyAssignable(Integer.TYPE, Short.TYPE);
		assertAsymmetricallyAssignable(Integer.TYPE, Byte.TYPE);
	}
	
	@Test
	public void isAssignableWithPrimitiveShort()
	{
		assertAssignable(Short.TYPE, Short.TYPE);
		assertAsymmetricallyAssignable(Short.TYPE, Byte.TYPE);
	}
	
	// JLS 4.10.2 Subtyping among Class and Interface Types
	
	/**
	 * The direct superclasses of C.
	 */
	@Test
	public void isAssignableWithDirectSuperclassFromParameterizedType()
	{
		assertAssignable(AbstractList.class, valueOf("ArrayList<Integer>"));
	}
	
	/**
	 * The direct superinterfaces of C.
	 */
	@Test
	public void isAssignableWithDirectSuperinterfaceFromParameterizedType()
	{
		assertAssignable(Collection.class, valueOf("List<Integer>"));
	}
	
	/**
	 * The type Object, if C is an interface type with no direct superinterfaces.
	 */
	@Test
	public void isAssignableWithObjectFromInterface()
	{
		assertAssignable(Object.class, Iterable.class);
	}
	
	/**
	 * The raw type C.
	 */
	@Test
	public void isAssignableWithRawTypeFromParameterizedType()
	{
		assertAssignable(List.class, valueOf("List<Integer>"));
	}
	
	// TODO: finish 4.10.2
	
	// JLS 4.10.3 Subtyping among Array Types
	
	/**
	 * If S and T are both reference types, then S[] >1 T[] iff S >1 T.
	 */
	@Test
	public void isAssignableWithArrayClassFromDirectSubtypeArrayClass()
	{
		assertAsymmetricallyAssignable(Number[].class, Integer[].class);
	}
	
	@Test
	public void isAssignableWithArrayClassFromIndirectSubtypeArrayClass()
	{
		assertAsymmetricallyAssignable(Object[].class, Integer[].class);
	}
	
	@Test
	public void isAssignableWithArrayClassFromGenericArrayType()
	{
		assertAssignable(Integer[].class, Types.genericArrayType(Integer.class));
	}
	
	@Test
	public void isAssignableWithArrayClassFromDirectSubtypeGenericArrayType()
	{
		assertAsymmetricallyAssignable(Number[].class, Types.genericArrayType(Integer.class));
	}
	
	@Test
	public void isAssignableWithArrayClassFromIndirectSubtypeGenericArrayType()
	{
		assertAsymmetricallyAssignable(Object[].class, Types.genericArrayType(Integer.class));
	}
	
	@Test
	public void isAssignableWithGenericArrayTypeFromDirectSubtypeGenericArrayType()
	{
		assertAsymmetricallyAssignable(Types.genericArrayType(Number.class), Types.genericArrayType(Integer.class));
	}
	
	@Test
	public void isAssignableWithGenericArrayTypeFromIndirectSubtypeGenericArrayType()
	{
		assertAsymmetricallyAssignable(Types.genericArrayType(Object.class), Types.genericArrayType(Integer.class));
	}
	
	@Test
	public void isAssignableWithGenericArrayTypeFromArrayClass()
	{
		assertAssignable(Types.genericArrayType(Integer.class), Integer[].class);
	}
	
	@Test
	public void isAssignableWithGenericArrayTypeFromDirectSubtypeArrayClass()
	{
		assertAsymmetricallyAssignable(Types.genericArrayType(Number.class), Integer[].class);
	}
	
	@Test
	public void isAssignableWithGenericArrayTypeFromIndirectSubtypeArrayClass()
	{
		assertAsymmetricallyAssignable(Types.genericArrayType(Object.class), Integer[].class);
	}
	
	/**
	 * Object >1 Object[].
	 */
	@Test
	public void isAssignableWithObjectFromObjectArrayClass()
	{
		assertAsymmetricallyAssignable(Object.class, Object[].class);
	}
	
	@Test
	public void isAssignableWithObjectFromArrayClass()
	{
		assertAsymmetricallyAssignable(Object.class, Integer[].class);
	}
	
	@Test
	public void isAssignableWithObjectFromObjectGenericArrayType()
	{
		assertAsymmetricallyAssignable(Object.class, Types.genericArrayType(Object.class));
	}
	
	@Test
	public void isAssignableWithObjectFromGenericArrayType()
	{
		assertAsymmetricallyAssignable(Object.class, Types.genericArrayType(Integer.class));
	}
	
	/**
	 * Cloneable >1 Object[].
	 */
	@Test
	public void isAssignableWithCloneableFromObjectArrayClass()
	{
		assertAsymmetricallyAssignable(Cloneable.class, Object[].class);
	}
	
	@Test
	public void isAssignableWithCloneableFromArrayClass()
	{
		assertAsymmetricallyAssignable(Cloneable.class, Integer[].class);
	}
	
	@Test
	public void isAssignableWithCloneableFromObjectGenericArrayType()
	{
		assertAsymmetricallyAssignable(Cloneable.class, Types.genericArrayType(Object.class));
	}
	
	@Test
	public void isAssignableWithCloneableFromGenericArrayType()
	{
		assertAsymmetricallyAssignable(Cloneable.class, Types.genericArrayType(Integer.class));
	}
	
	/**
	 * java.io.Serializable >1 Object[].
	 */
	@Test
	public void isAssignableWithSerializableFromObjectArrayClass()
	{
		assertAsymmetricallyAssignable(Serializable.class, Object[].class);
	}
	
	@Test
	public void isAssignableWithSerializableFromArrayClass()
	{
		assertAsymmetricallyAssignable(Serializable.class, Integer[].class);
	}
	
	@Test
	public void isAssignableWithSerializableFromObjectGenericArrayType()
	{
		assertAsymmetricallyAssignable(Serializable.class, Types.genericArrayType(Object.class));
	}
	
	@Test
	public void isAssignableWithSerializableFromGenericArrayType()
	{
		assertAsymmetricallyAssignable(Serializable.class, Types.genericArrayType(Integer.class));
	}
	
	/**
	 * If p is a primitive type, then Object >1 p[]. 
	 */
	@Test
	public void isAssignableWithObjectFromPrimitiveArray()
	{
		assertAsymmetricallyAssignable(Object.class, int[].class);
	}
	
	/**
	 * If p is a primitive type, then Cloneable >1 p[]. 
	 */
	@Test
	public void isAssignableWithCloneableFromPrimitiveArray()
	{
		assertAsymmetricallyAssignable(Cloneable.class, int[].class);
	}
	
	/**
	 * If p is a primitive type, then java.io.Serializable >1 p[]. 
	 */
	@Test
	public void isAssignableWithSerializableFromPrimitiveArray()
	{
		assertAsymmetricallyAssignable(Serializable.class, int[].class);
	}
	
	@Test(expected = NullPointerException.class)
	public void isAssignableWithNullSupertype()
	{
		assertAssignable(null, Integer.class);
	}
	
	@Test(expected = NullPointerException.class)
	public void isAssignableWithNullType()
	{
		assertAssignable(Integer.class, null);
	}
	
	/**
	 * Tests that classes are assignable to their direct superclasses.
	 * 
	 * {@literal Number <: Integer}
	 */
	@Test
	public void isAssignableWithClassFromDirectSubclass()
	{
		assertAsymmetricallyAssignable(Number.class, Integer.class);
	}
	
	/**
	 * Tests that classes are assignable to their indirect superclasses.
	 * 
	 * {@literal Object <: Integer}
	 */
	@Test
	public void isAssignableWithClassFromIndirectSubclass()
	{
		assertAsymmetricallyAssignable(Object.class, Integer.class);
	}
	
	/**
	 * Tests that parameterized types are assignable to their raw types.
	 * 
	 * {@literal List <: List<Integer>}
	 */
	@Test
	public void isAssignableWithClassFromParameterizedType()
	{
		assertAsymmetricallyAssignable(List.class, valueOf("List<Integer>"));
	}
	
	/**
	 * Tests that parameterized types are assignable if their raw types are directly assignable.
	 * 
	 * {@literal Collection<Integer> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithDirectlyAssignableParameterizedTypeRawTypes()
	{
		assertAsymmetricallyAssignable(valueOf("Collection<Integer>"), valueOf("List<Integer>"));
	}
	
	/**
	 * Tests that parameterized types are assignable if their raw types are indirectly assignable.
	 * 
	 * {@literal Collection<Integer> <: ArrayList<Integer>}
	 */
	@Test
	public void isAssignableWithIndirectlyAssignableParameterizedTypeRawTypes()
	{
		assertAsymmetricallyAssignable(valueOf("Collection<Integer>"), valueOf("ArrayList<Integer>"));
	}
	
	/**
	 * Tests that parameterized types are not assignable if their raw types are not assignable.
	 * 
	 * {@literal List<Integer> !<: Set<Integer>}
	 */
	@Test
	public void isAssignableWithUnassignableParameterizedTypeRawTypes()
	{
		assertUnassignable(valueOf("List<Integer>"), valueOf("Set<Integer>"));
		assertUnassignable(valueOf("Set<Integer>"), valueOf("List<Integer>"));
	}
	
	/**
	 * Tests that parameterized types are not assignable even if their type arguments are assignable.
	 * 
	 * {@literal List<Number> !<: List<Integer>}
	 */
	@Test
	public void isAssignableWithAssignableParameterizedTypeArguments()
	{
		assertUnassignable(valueOf("List<Number>"), valueOf("List<Integer>"));
		assertUnassignable(valueOf("List<Integer>"), valueOf("List<Number>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types.
	 * 
	 * {@literal List<?> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithWildcardParameterizedTypeFromParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<?>"), valueOf("List<Integer>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types on their upper bound.
	 * 
	 * {@literal List<? extends Number> <: List<Number>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? extends Number>"), valueOf("List<Number>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types within their upper bound.
	 * 
	 * {@literal List<? extends Number> <: List<Integer>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromInBoundsParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? extends Number>"), valueOf("List<Integer>"));
	}
	
	/**
	 * Tests that parameterized type arguments are not assignable to wildcard types outside of their upper bound.
	 * 
	 * {@literal List<? extends Number> !<: List<Object>}
	 */
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromOutOfBoundsParameterizedType()
	{
		assertUnassignable(valueOf("List<? extends Number>"), valueOf("List<Object>"));
		assertUnassignable(valueOf("List<Object>"), valueOf("List<? extends Number>"));
	}
	
	/**
	 * {@literal List<? extends Number> <: List<? extends Integer>}
	 */
	// TODO: fix
	@Ignore
	@Test
	public void isAssignableWithUpperBoundedWildcardParameterizedTypeFromInBoundsUpperBoundedWildcardParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? extends Number>"), valueOf("List<? extends Integer>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types on their lower bound.
	 * 
	 * {@literal List<? super Number> <: List<Number>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? super Number>"), valueOf("List<Number>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types within their lower bound.
	 * 
	 * {@literal List<? super Number> <: List<Object>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromInBoundsParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? super Number>"), valueOf("List<Object>"));
	}
	
	/**
	 * Tests that parameterized type arguments are assignable to wildcard types outside of their lower bound.
	 * 
	 * {@literal List<? super Number> !<: List<Integer>}
	 */
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromOutOfBoundsParameterizedType()
	{
		assertUnassignable(valueOf("List<? super Number>"), valueOf("List<Integer>"));
		assertUnassignable(valueOf("List<Integer>"), valueOf("List<? super Number>"));
	}
	
	/**
	 * {@literal List<? super Integer> <: List<? super Number>}
	 */
	// TODO: fix
	@Ignore
	@Test
	public void isAssignableWithLowerBoundedWildcardParameterizedTypeFromInBoundsLowerBoundedWildcardParameterizedType()
	{
		assertAsymmetricallyAssignable(valueOf("List<? super Integer>"), valueOf("List<? super Number>"));
	}
	
	/**
	 * Tests that classes are assignable to parameterized supertypes.
	 * 
	 * {@literal List<Integer> <: IntegerArrayList}
	 */
	@Test
	public void isAssignableWithParameterizedTypeFromClassWithActualTypeArguments()
	{
		assertAsymmetricallyAssignable(valueOf("List<Integer>"), IntegerArrayList.class);
	}
	
	@Test
	public void isAssignableWithUnboundedWildcardParameterizedTypeFromClass()
	{
		assertAsymmetricallyAssignable(valueOf("List<?>"), ArrayList.class);
	}
	
	@Test
	public void isAssignableWithUnboundedWildcardParameterizedTypeFromClassWithActualTypeArguments()
	{
		assertAsymmetricallyAssignable(valueOf("Map<?,?>"), StringsByIntegerHashMap.class);
	}
	
	/**
	 * Tests that unbounded type variables are assignable to Object.
	 * 
	 * {@literal Object <: T}
	 */
	@Test
	public void isAssignableWithObjectFromTypeVariableWithNoBounds()
	{
		assertAssignable(Object.class, Types.typeVariable(declaration, "T"));
	}

	/**
	 * Tests that type variables with a single bound are assignable to their bound. 
	 * 
	 * {@literal Number <: T extends Number}
	 */
	@Test
	public void isAssignableWithBoundFromTypeVariableWithBound()
	{
		assertAssignable(Number.class, Types.typeVariable(declaration, "T", Number.class));
	}
	
	/**
	 * Tests that type variables with a single bound are not assignable to subtypes of their bound.
	 * 
	 * {@literal Integer !<: T extends Number}
	 */
	@Test
	public void isAssignableWithTypeOutsideOfBoundFromTypeVariableWithBound()
	{
		assertUnassignable(Integer.class, Types.typeVariable(declaration, "T", Number.class));
	}
	
	/**
	 * Tests that type variables with a single bound are assignable to supertypes of their bound.
	 * 
	 * {@literal Number <: T extends Integer}
	 */
	@Test
	public void isAssignableWithTypeInsideOfBoundFromTypeVariableWithBound()
	{
		assertAssignable(Number.class, Types.typeVariable(declaration, "T", Integer.class));
	}
	
	/**
	 * Tests that type variables with multiple bounds are assignable to their bounds.
	 * 
	 * {@literal Number, Collection <: T extends Number & Collection}
	 */
	@Test
	public void isAssignableWithBoundsFromTypeVariableWithBounds()
	{
		TypeVariable<?> type = Types.typeVariable(declaration, "T", Number.class, Collection.class);

		assertAssignable(Number.class, type);
		assertAssignable(Collection.class, type);
	}
	
	/**
	 * Tests that type variables with multiple bounds are not assignable to supertypes of their bounds.
	 * 
	 * {@literal Integer, Thread !<: T extends Number & Runnable}
	 */
	@Test
	public void isAssignableWithTypeOutsideOfBoundsFromTypeVariableWithBounds()
	{
		TypeVariable<?> type = Types.typeVariable(declaration, "T", Number.class, Collection.class);
		
		assertUnassignable(Integer.class, type);
		assertUnassignable(List.class, type);
	}
	
	/**
	 * Tests that type variables with multiple bounds are assignable to subtypes of their bounds.
	 * 
	 * {@literal Number, Collection <: T extends Integer & List}
	 */
	@Test
	public void isAssignableWithTypeInsideOfBoundsFromTypeVariableWithBounds()
	{
		TypeVariable<?> type = Types.typeVariable(declaration, "T", Integer.class, List.class);
		
		assertAssignable(Number.class, type);
		assertAssignable(Collection.class, type);
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
	
	@Test
	public void getErasedTypeWithNull()
	{
		assertNull(TypeUtils.getErasedType(null));
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
	
	// getErasedReferenceType tests -------------------------------------------
	
	@Test
	public void getErasedReferenceTypeWithNull()
	{
		assertNull(TypeUtils.getErasedReferenceType(null));
	}
	
	@Test
	public void getErasedReferenceTypeWithClass()
	{
		assertEquals(Integer.class, TypeUtils.getErasedReferenceType(Integer.class));
	}
	
	@Test
	public void getErasedReferenceTypeWithClassArray()
	{
		assertEquals(Integer[].class, TypeUtils.getErasedReferenceType(Integer[].class));
	}
	
	@Test
	public void getErasedReferenceTypeWithTypeVariable()
	{
		assertEquals(Number.class, TypeUtils.getErasedReferenceType(Types.typeVariable(declaration, "T", Number.class,
			Comparable.class)));
	}
	
	@Test
	public void getErasedReferenceTypeWithGenericArrayType()
	{
		assertEquals(List[].class, TypeUtils.getErasedReferenceType(valueOf("List<Integer>[]")));
	}
	
	@Test
	public void getErasedReferenceTypeWithParameterizedType()
	{
		assertEquals(List.class, TypeUtils.getErasedReferenceType(valueOf("List<Integer>")));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void getErasedReferenceTypeWithWildcardType()
	{
		TypeUtils.getErasedReferenceType(Types.unboundedWildcardType());
	}
	
	// getRawType tests -------------------------------------------------------
	
	@Deprecated
	@Test
	public void getRawTypeWithNull()
	{
		assertNull(TypeUtils.getRawType(null));
	}
	
	@Deprecated
	@Test
	public void getRawTypeWithClass()
	{
		assertEquals(Integer.class, TypeUtils.getRawType(Integer.class));
	}
	
	@Deprecated
	@Test
	public void getRawTypeWithGenericArrayType()
	{
		assertEquals(List[].class, TypeUtils.getRawType(valueOf("List<Integer>[]")));
	}
	
	@Deprecated
	@Test
	public void getRawTypeWithParameterizedType()
	{
		assertEquals(List.class, TypeUtils.getRawType(valueOf("List<Integer>")));
	}
	
	// isArray tests ----------------------------------------------------------
	
	@Test
	public void isArrayWithNull()
	{
		assertFalse(TypeUtils.isArray(null));
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
	
	@Test
	public void getComponentTypeWithNull()
	{
		assertNull(TypeUtils.getComponentType(null));
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
	
	// getResolvedSuperclass tests ---------------------------------------------
	
	@Test
	public void getResolvedSuperclassWithPrimitive()
	{
		assertNull(TypeUtils.getResolvedSuperclass(int.class));
	}
	
	@Test
	public void getResolvedSuperclassWithPrimitiveArray()
	{
		assertEquals(Object.class, TypeUtils.getResolvedSuperclass(int[].class));
	}
	
	@Test
	public void getResolvedSuperclassWithObject()
	{
		assertNull(TypeUtils.getResolvedSuperclass(Object.class));
	}
	
	@Test
	public void getResolvedSuperclassWithClass()
	{
		assertEquals(Number.class, TypeUtils.getResolvedSuperclass(Integer.class));
	}
	
	@Test
	public void getResolvedSuperclassWithArrayClass()
	{
		assertEquals(Object.class, TypeUtils.getResolvedSuperclass(Integer[].class));
	}
	
	@Test
	public void getResolvedSuperclassWithTypeVariable()
	{
		assertNull(TypeUtils.getResolvedSuperclass(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void getResolvedSuperclassWithGenericArray()
	{
		assertEquals(Object.class, TypeUtils.getResolvedSuperclass(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void getResolvedSuperclassWithParameterizedType()
	{
		assertEquals(valueOf("AbstractList<Integer>"), TypeUtils.getResolvedSuperclass(valueOf("ArrayList<Integer>")));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void getResolvedSuperclassWithWildcardType()
	{
		TypeUtils.getResolvedSuperclass(Types.unboundedWildcardType());
	}
	
	@Test(expected = NullPointerException.class)
	public void getResolvedSuperclassWithNull()
	{
		TypeUtils.getResolvedSuperclass(null);
	}
	
	// getResolvedInterfaces tests --------------------------------------------
	
	@Test
	public void getResolvedInterfacesWithPrimitive()
	{
		assertArrayEquals(new Type[0], TypeUtils.getResolvedInterfaces(int.class));
	}
	
	@Test
	public void getResolvedInterfacesWithPrimitiveArray()
	{
		Type[] expectedInterfaces = new Type[] {
			Cloneable.class,
			Serializable.class
		};
		
		assertArrayEquals(expectedInterfaces, TypeUtils.getResolvedInterfaces(int[].class));
	}
	
	@Test
	public void getResolvedInterfacesWithClass()
	{
		Type[] expectedInterfaces = new Type[] {
			valueOf("java.lang.Comparable<Integer>")
		};
		
		assertArrayEquals(expectedInterfaces, TypeUtils.getResolvedInterfaces(Integer.class));
	}
	
	@Test
	public void getResolvedInterfacesWithArrayClass()
	{
		Type[] expectedInterfaces = new Type[] {
			Cloneable.class,
			Serializable.class
		};
		
		assertArrayEquals(expectedInterfaces, TypeUtils.getResolvedInterfaces(Integer[].class));
	}
	
	@Test
	public void getResolvedInterfacesWithTypeVariable()
	{
		assertArrayEquals(new Type[0], TypeUtils.getResolvedInterfaces(Types.typeVariable(declaration, "T")));
	}
	
	@Test
	public void getResolvedInterfacesWithGenericArray()
	{
		Type[] expectedInterfaces = new Type[] {
			Cloneable.class,
			Serializable.class
		};
		
		assertArrayEquals(expectedInterfaces, TypeUtils.getResolvedInterfaces(Types.genericArrayType(Integer.class)));
	}
	
	@Test
	public void getResolvedInterfacesWithParameterizedType()
	{
		Type[] expectedInterfaces = new Type[] {
			valueOf("List<Integer>"),
			RandomAccess.class,
			Cloneable.class,
			Serializable.class
		};
		
		assertArrayEquals(expectedInterfaces, TypeUtils.getResolvedInterfaces(valueOf("ArrayList<Integer>")));
	}
	
	@Test(expected = IllegalArgumentException.class)
	public void getResolvedInterfacesWithWildcardType()
	{
		TypeUtils.getResolvedInterfaces(Types.unboundedWildcardType());
	}
	
	@Test(expected = NullPointerException.class)
	public void getResolvedInterfacesWithNull()
	{
		TypeUtils.getResolvedInterfaces(null);
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
	
	private static void assertAsymmetricallyAssignable(Type supertype, Type type)
	{
		assertAssignable(supertype, type);
		assertUnassignable(type, supertype);
	}
	
	private static void assertAssignable(Type supertype, Type type)
	{
		assertTrue("Expected " + type + " assignable to " + supertype, TypeUtils.isAssignable(supertype, type));
	}
	
	private static void assertUnassignable(Type supertype, Type type)
	{
		assertFalse("Expected " + type + " not assignable to " + supertype, TypeUtils.isAssignable(supertype, type));
	}
	
	private Type valueOf(String typeName)
	{
		return Types.valueOf(typeName, valueOfImports);
	}
}
