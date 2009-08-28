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
 * Provides a generic type literal.
 * <p>
 * This class captures the actual type argument used when subclassed. This allows it to be referenced as a type
 * parameter at compile time and also makes it available at run time. It is intended to be used as follows:
 * <p>
 * {@code Generic<List<String>> listStringType = new Generic<List<String>>() }<code>{};</code>
 * <p>
 * This allows generic type literals to be used in a simple manner as standard class literals. For example, consider the
 * following generic method signature:
 * <p>
 * {@code <T> void add(T element, Class<T> type)}
 * <p>
 * A problem arises when {@code <T>} is a generic type, such as {@code List<String>}, since {@code List<String>.class}
 * produces a compile time error. Use of this class can mitigate this problem:
 * <p>
 * {@code <T> void add(T element, Generic<T> type)}
 * <p>
 * Which can then be invoked as follows:
 * <p>
 * {@code add(new ArrayList<String>(), new Generic<List<String>>() }<code>{});</code>
 * 
 * @author Mark Hobson
 * @version $Id: Generic.java 2 2009-02-02 22:28:39Z markhobson $
 * @param <T> the type that this generic type literal represents
 * @see Generics
 * @see <a href="http://gafter.blogspot.com/2006/12/super-type-tokens.html">Neal Gafter's blog: Super Type Tokens</a>
 */
public abstract class Generic<T>
{
	// TODO: make serializable?
	
	// classes ----------------------------------------------------------------
	
	private static final class DefaultGeneric<T> extends Generic<T>
	{
		public DefaultGeneric(Type type)
		{
			super(type);
		}
	}
	
	// fields -----------------------------------------------------------------
	
	private final Type type;
	
	// constructors -----------------------------------------------------------
	
	protected Generic()
	{
		Type type = getActualTypeArgument();
		
		validateType(type);
		
		this.type = type;
	}
	
	Generic(Type type)
	{
		validateType(type);
		
		this.type = type;
	}
	
	// public methods ---------------------------------------------------------
	
	public Type getType()
	{
		return type;
	}
	
	@SuppressWarnings("unchecked")
	public Class<? super T> getRawType()
	{
		return (Class<? super T>) TypeUtils.getRawType(type);
	}
	
	public String toUnqualifiedString()
	{
		return TypeUtils.toUnqualifiedString(type);
	}
	
	public static <T> Generic<T> get(Class<T> klass)
	{
		return new DefaultGeneric<T>(klass);
	}
	
	public static Generic<?> get(Type type)
	{
		return new DefaultGeneric<Object>(type);
	}
	
	@SuppressWarnings("unchecked")
	public static <T> Generic<? extends T> get(Class<T> rawType, Type... actualTypeArguments)
	{
		ParameterizedType paramType = Types.parameterizedType(rawType, actualTypeArguments);
		
		return (Generic<? extends T>) get(paramType);
	}
	
	public static Generic<?> valueOf(String typeName)
	{
		return get(Types.valueOf(typeName));
	}
	
	// Object methods ---------------------------------------------------------
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public int hashCode()
	{
		return type.hashCode();
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean equals(Object object)
	{
		if (!(object instanceof Generic))
		{
			return false;
		}
		
		Generic<?> generic = (Generic<?>) object;
		
		return type.equals(generic.getType());
	}
	
	/**
	 * {@inheritDoc}
	 */
	@Override
	public String toString()
	{
		return TypeUtils.toString(type);
	}
	
	// private methods --------------------------------------------------------
	
	private static void validateType(Type type)
	{
		Utils.checkNotNull(type, "type");
		Utils.checkFalse(type instanceof GenericArrayType, "Generic array types are not supported: ", type);
		Utils.checkFalse(type instanceof TypeVariable, "Type variables are not supported: ", type);
		Utils.checkFalse(type instanceof WildcardType, "Wildcard types are not supported: ", type);
		Utils.checkTrue(type instanceof Class || type instanceof ParameterizedType, "Unsupported type: ", type);
	}
	
	private Type getActualTypeArgument()
	{
		if (getClass().getSuperclass() != Generic.class)
		{
			throw new IllegalStateException("Generic must only be subclassed once");
		}
		
		Type superclass = getClass().getGenericSuperclass();
		
		return ((ParameterizedType) superclass).getActualTypeArguments()[0];
	}
}
