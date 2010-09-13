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
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.lang.reflect.WildcardType;

/**
 * Provides utility methods for working with types.
 * 
 * @author Mark Hobson
 * @version $Id: TypeUtils.java 3 2009-02-03 21:40:20Z markhobson $
 */
public final class TypeUtils
{
	// constructors -----------------------------------------------------------
	
	private TypeUtils()
	{
		throw new AssertionError();
	}
	
	// public methods ---------------------------------------------------------
	
	public static boolean isAssignable(Type supertype, Type type)
	{
		Utils.checkNotNull(supertype, "supertype");
		Utils.checkNotNull(type, "type");
		
		boolean assignable;
		
		if (supertype.equals(type))
		{
			assignable = true;
		}
		else if (supertype instanceof Class && type instanceof Class)
		{
			assignable = ((Class<?>) supertype).isAssignableFrom((Class<?>) type);
		}
		else if (supertype instanceof Class && type instanceof ParameterizedType)
		{
			assignable = isAssignable(supertype, ((ParameterizedType) type).getRawType());
		}
		else if (supertype instanceof ParameterizedType && type instanceof ParameterizedType)
		{
			assignable = isAssignable((ParameterizedType) supertype, (ParameterizedType) type);
		}
		else if (supertype instanceof WildcardType)
		{
			assignable = isAssignable((WildcardType) supertype, type);
		}
		else if (type instanceof Class)
		{
			assignable = isAssignable(supertype, (Class<?>) type);
		}
		else
		{
			assignable = false;
		}
		
		return assignable;
	}
	
	public static boolean isInstance(Type type, Object object)
	{
		return getRawType(type).isInstance(object);
	}
	
	/**
	 * Gets the erased type of the specified type.
	 * 
	 * @param type
	 *            the type to perform erasure on
	 * @return the erased type, never a parameterized type nor a type variable
	 * @see <a href="http://java.sun.com/docs/books/jls/third_edition/html/typesValues.html#4.6">4.6 Type Erasure</a>
	 */
	public static Type getErasedType(Type type)
	{
		Utils.checkNotNull(type, "type");
		
		Type erasedType;
		
		// the erasure of a parameterized type G<T1, ... ,Tn> is |G|
		if (type instanceof ParameterizedType)
		{
			Type rawType = ((ParameterizedType) type).getRawType();
			
			erasedType = getErasedType(rawType);
		}
		// TODO: the erasure of a nested type T.C is |T|.C
		// the erasure of an array type T[] is |T|[]
		else if (isArray(type))
		{
			Type componentType = getComponentType(type);
			Type erasedComponentType = getErasedType(componentType);
			
			erasedType = getArrayType(erasedComponentType);
		}
		// the erasure of a type variable is the erasure of its leftmost bound 
		else if (type instanceof TypeVariable)
		{
			Type[] bounds = ((TypeVariable<?>) type).getBounds();
			
			erasedType = getErasedType(bounds[0]);
		}
		// the erasure of every other type is the type itself
		else
		{
			erasedType = type;
		}
		
		return erasedType;
	}
	
	public static Class<?> getRawType(Type type)
	{
		Utils.checkNotNull(type, "type");
		
		Class<?> rawType;
		
		if (type instanceof Class)
		{
			rawType = (Class<?>) type;
		}
		else if (type instanceof GenericArrayType)
		{
			Type componentType = ((GenericArrayType) type).getGenericComponentType();
			
			Class<?> rawComponentType = getRawType(componentType);
			
			rawType = ClassUtils.getArrayType(rawComponentType);
		}
		else if (type instanceof ParameterizedType)
		{
			rawType = getRawType(((ParameterizedType) type).getRawType());
		}
		else
		{
			// TODO: support TypeVariables and WildcardTypes
			
			throw new IllegalArgumentException("Cannot obtain raw type from " + type);
		}
		
		return rawType;
	}
	
	public static boolean isArray(Type type)
	{
		Utils.checkNotNull(type, "type");
		
		boolean array;
		
		if (type instanceof Class)
		{
			array = ((Class<?>) type).isArray();
		}
		else if (type instanceof GenericArrayType)
		{
			array = true;
		}
		else
		{
			array = false;
		}
		
		return array;
	}
	
	public static Type getComponentType(Type type)
	{
		Utils.checkNotNull(type, "type");
		
		Type componentType;
		
		if (type instanceof Class)
		{
			Class<?> klass = (Class<?>) type;
			
			if (klass.isArray())
			{
				componentType = klass.getComponentType();
			}
			else
			{
				componentType = null;
			}
		}
		else if (type instanceof GenericArrayType)
		{
			componentType = ((GenericArrayType) type).getGenericComponentType();
		}
		else
		{
			componentType = null;
		}
		
		return componentType;
	}
	
	public static Type getArrayType(Type componentType)
	{
		Utils.checkNotNull(componentType, "componentType");
		
		Type arrayType;
		
		if (componentType instanceof Class)
		{
			arrayType = ClassUtils.getArrayType((Class<?>) componentType);
		}
		else
		{
			arrayType = Types.genericArrayType(componentType);
		}
		
		return arrayType;
	}
	
	public static boolean isSimpleParameterizedType(Type type, Class<?> rawType)
	{
		Utils.checkNotNull(type, "type");
		Utils.checkNotNull(rawType, "rawType");
		
		if (!(type instanceof ParameterizedType))
		{
			return false;
		}
		
		ParameterizedType paramType = (ParameterizedType) type;
		
		Type paramRawType = paramType.getRawType();
		
		if (!(paramRawType instanceof Class))
		{
			return false;
		}
		
		Class<?> paramRawClass = (Class<?>) paramRawType;
		
		if (!rawType.isAssignableFrom(paramRawClass))
		{
			return false;
		}
		
		Type[] typeArgs = paramType.getActualTypeArguments();
		
		return (typeArgs.length == 1);
	}
	
	public static Type getActualTypeArgument(Type type)
	{
		Utils.checkNotNull(type, "type");
		
		ParameterizedType paramType = (ParameterizedType) type;
		
		Type[] typeArgs = paramType.getActualTypeArguments();
		
		Utils.checkTrue(typeArgs.length == 1, "type must be a ParameterizedType with one actual type argument: ", type);
		
		return typeArgs[0];
	}
	
	public static String toString(Type type)
	{
		return toString(type, ClassSerializers.QUALIFIED);
	}
	
	public static String toString(Type type, ClassSerializer serializer)
	{
		String value;
		
		if (type instanceof Class)
		{
			Class<?> klass = (Class<?>) type;
			
			if (klass.isArray())
			{
				value = toString(klass.getComponentType(), serializer) + "[]";
			}
			else
			{
				value = serializer.toString(klass);
			}
		}
		else if (type instanceof TypeVariable)
		{
			value = DefaultTypeVariable.toString((TypeVariable<?>) type, serializer);
		}
		else if (type instanceof GenericArrayType)
		{
			value = DefaultGenericArrayType.toString((GenericArrayType) type, serializer);
		}
		else if (type instanceof ParameterizedType)
		{
			value = DefaultParameterizedType.toString((ParameterizedType) type, serializer);
		}
		else if (type instanceof WildcardType)
		{
			value = DefaultWildcardType.toString((WildcardType) type, serializer);
		}
		else
		{
			value = String.valueOf(type);
		}
		
		return value;
	}

	public static String toUnqualifiedString(Type type)
	{
		return toString(type, ClassSerializers.UNQUALIFIED);
	}
	
	// package methods --------------------------------------------------------
	
	static StringBuilder appendBounds(StringBuilder builder, Type[] bounds, ClassSerializer serializer)
	{
		for (int i = 0; i < bounds.length; i++)
		{
			if (i > 0)
			{
				builder.append(" & ");
			}
			
			builder.append(toString(bounds[i], serializer));
		}
		
		return builder;
	}
	
	// private methods --------------------------------------------------------
	
	private static boolean isAssignable(ParameterizedType supertype, ParameterizedType type)
	{
		if (!isAssignable(supertype.getRawType(), type.getRawType()))
		{
			return false;
		}
		
		Type[] supertypeArgs = supertype.getActualTypeArguments();
		Type[] typeArgs = type.getActualTypeArguments();
		
		if (supertypeArgs.length != typeArgs.length)
		{
			return false;
		}
		
		for (int i = 0; i < supertypeArgs.length; i++)
		{
			Type supertypeArg = supertypeArgs[i];
			Type typeArg = typeArgs[i];
			
			if (!supertypeArg.equals(typeArg)
				&& !(supertypeArg instanceof WildcardType && isAssignable((WildcardType) supertypeArg, typeArg)))
			{
				return false;
			}
		}
		
		return true;
	}
	
	private static boolean isAssignable(WildcardType supertype, Type type)
	{
		for (Type upperBound : supertype.getUpperBounds())
		{
			if (!isAssignable(upperBound, type))
			{
				return false;
			}
		}
		
		for (Type lowerBound : supertype.getLowerBounds())
		{
			if (!isAssignable(type, lowerBound))
			{
				return false;
			}
		}
		
		return true;
	}
	
	private static boolean isAssignable(Type supertype, Class<?> type)
	{
		Type genericSuperclass = type.getGenericSuperclass();
		
		if (genericSuperclass != null && isAssignable(supertype, genericSuperclass))
		{
			return true;
		}
		
		for (Type interphace : type.getGenericInterfaces())
		{
			if (isAssignable(supertype, interphace))
			{
				return true;
			}
		}
		
		return false;
	}
}
