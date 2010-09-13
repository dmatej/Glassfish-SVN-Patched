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
import java.lang.reflect.GenericDeclaration;
import java.lang.reflect.MalformedParameterizedTypeException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Factory for creating types.
 * 
 * @author Mark Hobson
 * @version $Id: Types.java 2 2009-02-02 22:28:39Z markhobson $
 * @see Type
 */
public final class Types
{
	// constructors -----------------------------------------------------------
	
	private Types()
	{
		throw new AssertionError();
	}
	
	// public methods ---------------------------------------------------------
	
	/**
	 * Creates a type variable for the specified declaration, name and bounds.
	 * 
	 * @param <D>
	 *            the type of generic declaration that declared the type variable
	 * @param declaration
	 *            the generic declaration that declared the type variable
	 * @param name
	 *            the name of the type variable
	 * @param bounds
	 *            the upper bounds of the type variable
	 * @return the type variable
	 */
	public static <D extends GenericDeclaration> TypeVariable<D> typeVariable(D declaration, String name,
		Type... bounds)
	{
		return new DefaultTypeVariable<D>(declaration, name, bounds);
	}
	
	/**
	 * Creates a generic array type for the specified component type.
	 * 
	 * @param componentType
	 *            the component type
	 * @return the generic array type
	 */
	public static GenericArrayType genericArrayType(Type componentType)
	{
		return new DefaultGenericArrayType(componentType);
	}
	
	/**
	 * Creates a parameterized type for the specified raw type and actual type arguments.
	 * 
	 * @param rawType
	 *            the raw type
	 * @param actualTypeArguments
	 *            the actual type arguments
	 * @return the parameterized type
	 * @throws MalformedParameterizedTypeException
	 *             if the number of actual type arguments differs from those defined on the raw type
	 */
	public static ParameterizedType parameterizedType(Class<?> rawType, Type... actualTypeArguments)
	{
		return new DefaultParameterizedType(null, rawType, actualTypeArguments);
	}
	
	/**
	 * Creates an unbounded wildcard type.
	 * 
	 * @return the wildcard type
	 */
	public static WildcardType unboundedWildcardType()
	{
		return wildcardType(null, null);
	}
	
	/**
	 * Creates a wildcard type with the specified upper bound.
	 * 
	 * @param upperBound
	 *            the upper bound type
	 * @return the wildcard type
	 */
	public static WildcardType upperBoundedWildcardType(Type upperBound)
	{
		Utils.checkNotNull(upperBound, "upperBound");
		
		return wildcardType(new Type[] {upperBound}, null);
	}
	
	/**
	 * Creates a wildcard type with the specified lower bound.
	 * 
	 * @param lowerBound
	 *            the lower bound type
	 * @return the wildcard type
	 */
	public static WildcardType lowerBoundedWildcardType(Type lowerBound)
	{
		Utils.checkNotNull(lowerBound, "lowerBound");
		
		return wildcardType(null, new Type[] {lowerBound});
	}
	
	/**
	 * Returns a type that corresponds to the specified string.
	 * 
	 * @param typeName
	 *            the string to be parsed
	 * @return the type
	 */
	public static Type valueOf(String typeName)
	{
		return valueOf(typeName, Collections.<String>emptySet());
	}
	
	/**
	 * Returns a type that corresponds to the specified string using the specified import context.
	 * 
	 * @param typeName
	 *            the string to be parsed
	 * @param imports
	 *            the fully qualified class names to use when an unqualified class name is encountered
	 * @return the type
	 * @throws IllegalArgumentException
	 *             if the import context contains duplicate entries for an unqualified class name
	 */
	public static Type valueOf(String typeName, Set<String> imports)
	{
		Map<String, String> importMap = new HashMap<String, String>();
		
		for (String className : imports)
		{
			String unqualifiedClassName = ClassUtils.getUnqualifiedClassName(className);
			
			if (importMap.containsKey(unqualifiedClassName))
			{
				throw new IllegalArgumentException("Duplicate imports: " + importMap.get(unqualifiedClassName)
					+ " and " + className);
			}
			
			importMap.put(unqualifiedClassName, className);
		}
		
		return valueOf(typeName, importMap);
	}
	
	// private methods --------------------------------------------------------
	
	private static WildcardType wildcardType(Type[] upperBounds, Type[] lowerBounds)
	{
		return new DefaultWildcardType(upperBounds, lowerBounds);
	}
	
	private static Type valueOf(String typeName, Map<String, String> imports)
	{
		// handle arrays
		
		if (typeName.endsWith("[]"))
		{
			String componentName = typeName.substring(0, typeName.length() - "[]".length());
			
			Type componentType = valueOf(componentName, imports);
			
			return TypeUtils.getArrayType(componentType);
		}
		
		// handle wildcards
		
		if (typeName.startsWith("?"))
		{
			return parseWildcardType(typeName, imports);
		}
		
		// handle classes
		
		int argStart = typeName.indexOf('<');
		
		if (argStart == -1)
		{
			return parseClass(typeName, imports);
		}
		
		// handle parameterized types
		
		int argEnd = typeName.lastIndexOf('>');
		
		if (argEnd == -1)
		{
			throw new IllegalArgumentException("Mismatched type argument delimiters: " + typeName);
		}
		
		String rawTypeName = typeName.substring(0, argStart);
		Class<?> rawType = parseClass(rawTypeName, imports);
		
		String[] actualTypeArgumentNames = typeName.substring(argStart + 1, argEnd).split(",");
		Type[] actualTypeArguments = new Type[actualTypeArgumentNames.length];
		
		for (int i = 0; i < actualTypeArgumentNames.length; i++)
		{
			actualTypeArguments[i] = valueOf(actualTypeArgumentNames[i], imports);
		}
		
		return parameterizedType(rawType, actualTypeArguments);
	}
	
	private static Class<?> parseClass(String className, Map<String, String> imports)
	{
		Class<?> klass = parseClass(className);
		
		if (klass != null)
		{
			return klass;
		}
		
		if (!className.contains(".") && imports.containsKey(className))
		{
			String qualifiedClassName = imports.get(className);
			
			klass = parseClass(qualifiedClassName);
			
			if (klass != null)
			{
				return klass;
			}
		}
		
		throw new IllegalArgumentException("Class not found: " + className);
	}
	
	private static Class<?> parseClass(String className)
	{
		try
		{
			return Class.forName(className);
		}
		catch (ClassNotFoundException exception)
		{
			return null;
		}
	}
	
	private static WildcardType parseWildcardType(String typeName, Map<String, String> imports)
	{
		Type[] upperBounds;
		Type[] lowerBounds;
		
		if ("?".equals(typeName))
		{
			upperBounds = null;
			lowerBounds = null;
		}
		else if (typeName.startsWith("? extends "))
		{
			String upperBoundName = typeName.substring("? extends ".length());
			Type upperBound = valueOf(upperBoundName, imports);
			
			upperBounds = new Type[] {upperBound};
			lowerBounds = null;
		}
		else if (typeName.startsWith("? super "))
		{
			String lowerBoundName = typeName.substring("? super ".length());
			Type lowerBound = valueOf(lowerBoundName, imports);
			
			upperBounds = null;
			lowerBounds = new Type[] {lowerBound};
		}
		else
		{
			throw new IllegalArgumentException("Invalid wildcard type: " + typeName);
		}
		
		return wildcardType(upperBounds, lowerBounds);
	}
}
