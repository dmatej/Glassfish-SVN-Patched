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

import java.util.Collection;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;

/**
 * Factory for creating common generics.
 * 
 * @author Mark Hobson
 * @version $Id: Generics.java 2 2009-02-02 22:28:39Z markhobson $
 * @see Generic
 */
@SuppressWarnings("unchecked")
public final class Generics
{
	// constructors -----------------------------------------------------------
	
	private Generics()
	{
		throw new AssertionError();
	}
	
	// public methods ---------------------------------------------------------
	
	public static <T> Generic<Comparator<T>> comparator(Class<T> type)
	{
		return (Generic<Comparator<T>>) Generic.get(Comparator.class, type);
	}
	
	public static <E> Generic<Enumeration<E>> enumeration(Class<E> elementClass)
	{
		return (Generic<Enumeration<E>>) Generic.get(Enumeration.class, elementClass);
	}
	
	public static <E> Generic<Iterator<E>> iterator(Class<E> elementClass)
	{
		return (Generic<Iterator<E>>) Generic.get(Iterator.class, elementClass);
	}
	
	public static <E> Generic<ListIterator<E>> listIterator(Class<E> elementClass)
	{
		return (Generic<ListIterator<E>>) Generic.get(ListIterator.class, elementClass);
	}
	
	public static <E> Generic<Collection<E>> collection(Class<E> elementClass)
	{
		return (Generic<Collection<E>>) Generic.get(Collection.class, elementClass);
	}
	
	public static <E> Generic<Set<E>> set(Class<E> elementClass)
	{
		return (Generic<Set<E>>) Generic.get(Set.class, elementClass);
	}
	
	public static <E> Generic<SortedSet<E>> sortedSet(Class<E> elementClass)
	{
		return (Generic<SortedSet<E>>) Generic.get(SortedSet.class, elementClass);
	}
	
	public static <E> Generic<List<E>> list(Class<E> elementClass)
	{
		return (Generic<List<E>>) Generic.get(List.class, elementClass);
	}
	
	public static <K, V> Generic<Map<K, V>> map(Class<K> keyClass, Class<V> valueClass)
	{
		return (Generic<Map<K, V>>) Generic.get(Map.class, keyClass, valueClass);
	}
	
	public static <K, V> Generic<SortedMap<K, V>> sortedMap(Class<K> keyClass, Class<V> valueClass)
	{
		return (Generic<SortedMap<K, V>>) Generic.get(SortedMap.class, keyClass, valueClass);
	}
	
	public static <E> Generic<Queue<E>> queue(Class<E> elementClass)
	{
		return (Generic<Queue<E>>) Generic.get(Queue.class, elementClass);
	}
}
