/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.AbstractQueue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.ConcurrentModificationException;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.RandomAccess;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.myfaces.trinidad.component.CompositeIterator;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;

/**
 * This class contains Collection utilities.
 */
public final class CollectionUtils
{
  private CollectionUtils()
  {
    // no-op
  }
  
  /**
   * Returns an ArrayList containing all of the elements of the
   * Iterator
   * @param iterator Iterator to copy the contexts of
   * @return an ArrayList containing a copy of the iterator contents
   */
  public static <T> ArrayList<T> arrayList(Iterator<T> iterator)
  {
    ArrayList<T> arrayList = new ArrayList<T>();
    
    while (iterator.hasNext())
      arrayList.add(iterator.next());
    
    return arrayList;
  }

  /**
   * Returns an array containing all of the elements of the
   * Iterator
   * @param iterator Iterator to copy the contexts of
   * @return an array containing a copy of the iterator contents
   */
  public static <T> T[] toArray(Iterator<? extends T> iterator, Class<T> type)
  {
    if (iterator.hasNext())
    {
      Collection arrayList = arrayList(iterator);
      T[] outArray = (T[])Array.newInstance(type, arrayList.size());
    
      return (T[])arrayList.toArray(outArray);
    }
    else
    {
      // optimize empty iterator case
      return (T[])Array.newInstance(type, 0);
    }
  }

  /**
   * Returns an empty, unmodifiable, Serializable Queue.
   * @return an empty, unmodifiable, Serializable Queue.
   */
  public static <T> Queue<T> emptyQueue()
  {
    return (Queue<T>)_EMPTY_QUEUE;
  }

  /**
   * Returns an empty, unmodifiable, Iterator.
   * @return an empty, unmodifiable, Iterator.
   */
  public static <T> Iterator<T> emptyIterator()
  {
    return (Iterator<T>)_EMPTY_ITERATOR;
  }

  /**
   * Returns an empty, unmodifiable, ListIterator.
   * @return an empty, unmodifiable, ListIterator.
   */
  public static <T> ListIterator<T> emptyListIterator()
  {
    return (ListIterator<T>)_EMPTY_LIST_ITERATOR;
  }
  
  /**
   * Returns a minimal Set containing the elements passed in.  There is no guarantee that the
   * returned set is mutable or immutable.
   * @param <T>
   * @param a The elements to add to the Set
   * @return A set containing the elements passed in.
   * @see #asUnmodifiableSet
   */
  public static <T> Set<T> asSet(T... a)
  {
    return _asSet(a, false);
  }

  /**
   * Returns an unmodifiable Set containing the elements passed in.
   * @param <T>
   * @param a The elements to add to the Set
   * @return A set containing the elements passed in.
   * @see #asSet
   */
  public static <T> Set<T> asUnmodifiableSet(T... a)
  {
    return _asSet(a, true);
  }

  /**
   * Returns an unmodifiable versions of the Set of Enums.  If the contents of the set are known
   * to be unmodifiable by the caller in any way, the set itself will be retured, otherwise an
   * unmodifiable copy of the Set will be returned.
   * @param s Set to get the tamper-proof version of
   * @return An unmodifiable tamper-proof version of the set
   */
  public static <E extends Enum<E>> Set<E> unmodifiableCopyOfEnumSet(Set<E> s)
  {
    Class<? extends Set> copyClass = s.getClass();

    if ((_EMPTY_SET == copyClass) || (_SINGLETON_SET == copyClass))
    {
      // these classes are already unmodifiable, so just return
      return s;
    }
    else
    {
      return Collections.unmodifiableSet(EnumSet.copyOf(s));
    }
  }

  private static <T> Set<T> _asSet(T[] a, boolean makeImmutable)
  {
    int count = (a != null) ? a.length : 0;

    Set<T> outSet;
    
    if (count == 0)
    {
      outSet = Collections.emptySet();
    }
    else
    {
      if (count == 1)
      {
        outSet = Collections.singleton(a[0]);
      }
      else
      {
        // make the initial size big enough that we don't have to rehash to fit the array, for
        // the .75 load factor we pass in
        int initialSize = (int)Math.ceil(count / .75d);
        
        outSet = new HashSet<T>(initialSize, .75f);
        
        for (int i = 0; i < count ; i++)
        {
          outSet.add(a[i]);
        } 
        
        if (makeImmutable)
        {
          outSet = Collections.unmodifiableSet(outSet);
        }
      }
    }
    
    return outSet;
  }

  /**
   * Given two disjoint sets, returns a live composition of the two Sets that maintains
   * the disjoint invariant.  If both Sets are Serializable, the returned
   * implementation will be Serializable as well.  The returned Set implementation is
   * not thread safe.
   * @param primarySet The Set that modifications will be applied to
   * @param secondarySet The other Set.  Modifications will be applied in response to changes
   * to the primary set to ensure that the disjoint invariant is maintained
   * @return The composition of the two disjoint Sets
   * @throws NullPointerException of primarySet or secondarySet are <code>null</code>
   * @see #overlappingCompositeSet
   */
  public static <T> Set<T> compositeSet(Set<T> primarySet, Set<T> secondarySet)
  {
    if ((primarySet instanceof Serializable) && (secondarySet instanceof Serializable))
      return new SerializableFixedCompositeSet<T>(primarySet, secondarySet);
    else
      return new FixedCompositeSet<T>(primarySet, secondarySet);
  }

  /**
   * Given two possibly overlapping sets, returns a live composition of the two Sets.
   * If both Sets are Serializable, the returned
   * implementation will be Serializable as well.  The lack of the disjoint invariant makes
   * operations such as calculating the size of the Set expensive.  If the disjoint invariant
   * can be guaranteed, <code>compositeSet</code> should be used instead.
   * The returned Set implementation is
   * not thread safe.
   * @param primarySet The Set that modifications will be applied to
   * @param secondarySet The other Set.  If a removal is performed on the primarySet, it will
   * also be applied to the secondarySet to ensure that the element is logically removed from the
   * Set.
   * @return The composition of the two possibly overallping Sets
   * @throws NullPointerException of primarySet or secondarySet are <code>null</code>
   * @see #compositeSet
   */
  public static <T> Set<T> overlappingCompositeSet(Set<T> primarySet, Set<T> secondarySet)
  {
    if ((primarySet instanceof Serializable) && (secondarySet instanceof Serializable))
      return new SerializableLenientFixedCompositeSet<T>(primarySet, secondarySet);
    else
      return new LenientFixedCompositeSet<T>(primarySet, secondarySet);
  }

  /**
   * Returns a Collection based on the passed in Collection <code>c</code>,
   * guaranteed to be Serializable. If <code>c</code> is Serializable,
   * <code>c</code> will be returned, otherwise, <code>c</code> will be
   * wrapped in a Collection that implements Serializable and upon
   * Serialization the contents of <code>c</code> will be copied into
   * the result.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>c</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * @param c The Collection to get a Serializable version of
   * @return A Serializable version of Collection <code>c</code>
   * @see #getSerializableList
   */
  public static <T> Collection<T> getSerializableCollection(Collection<T> c)
  {
    if (c instanceof Serializable)
      return c;
    else
      return new SerializableCollection<T>(c);
  }

  /**
   * Returns a List based on the passed in List <code>l</code>,
   * guaranteed to be Serializable. List <code>l</code> will be
   * wrapped in a List that implements Serializable and upon
   * Serialization the contents of <code>l</code> will be copied into
   * the result.
   * <p>
   * If <code>l</code> implements RandomAccess, any returned List will also
   * implement RandomAccess.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>l</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * <p>
   * Code that calls List.subList() and needs the result to be Serializable should always
   * use <code>newSerializableList</code> rather than <code>getSerializableList</code> because
   * the <code>java.util.Collections</code> implementations of <code>checkedList</code>,
   * <code>unmodifiableList</code>, and <code>synchronizedList</code> all lie and always implement
   * Serializable, regardless of the serializability of their backing List.
   * @param l The List to get a Serializable version of
   * @return A Serializable version of List <code>l</code>
   * @see #getSerializableList
   * @see #getSerializableCollection
   */
  public static <T> List<T> newSerializableList(List<T> l)
  {
    if (l instanceof RandomAccess)
    {
      return new SerializableRandomAccessList<T>(l);
    }
    else
    {
      return new SerializableList<T>(l);
    }
  }

  /**
   * Returns a List based on the passed in List <code>l</code>,
   * guaranteed to be Serializable. If <code>l</code> is Serializable,
   * <code>l</code> will be returned, otherwise, <code>l</code> will be
   * wrapped in a List that implements Serializable and upon
   * Serialization the contents of <code>l</code> will be copied into
   * the result.
   * <p>
   * If <code>l</code> implements RandomAccess, any returned List will also
   * implement RandomAccess.
   * <p>
   * The results is very similar to creating a new ArrayList with the
   * contents of <code>l</code>, but no wrapper is created unless necessary
   * and the actual creation of the Serializable copy is deferred until
   * Serialization occurs.
   * <p>
   * Code that calls List.subList() and needs the result to be Serializable should always
   * use <code>newSerializableList</code> rather than <code>getSerializableList</code> because
   * the <code>java.util.Collections</code> implementations of <code>checkedList</code>,
   * <code>unmodifiableList</code>, and <code>synchronizedList</code> all lie and always implement
   * Serializable, regardless of the serializability of their backing List.
   * @param l The List to get a Serializable version of
   * @return A Serializable version of List <code>l</code>
   * @see #newSerializableList
   * @see #getSerializableCollection
   */
  public static <T> List<T> getSerializableList(List<T> l)
  {
    // because we can't trust the implementations of the checked, unmodifiable, and synchronized
    // versions, always create a Serializable wrapper if we see one of these classes
    if ((l instanceof Serializable) &&
         !_CHECKED_LIST.isInstance(l) &&
         !_UNMODIFIABLE_LIST.isInstance(l) &&
         !_SYNCHRONIZED_LIST.isInstance(l))
      return l;
    else
    {
      return newSerializableList(l);
    }
  }

  /**
   * Interface for trapping mutations to a Map.
   * @param <K> the type of the keys of the Map that MapMutationHooks are associated with
   * @param <V> the type of the values of the Map that MapMutationHooks are associated with
   * @see #newMutationHookedMap
   */
  public interface MapMutationHooks<K, V>
  {
    /**
     * Called when the associated Map of the MapMutationHooks is written to
     * @param map   Map the write occurred on
     * @param key   key of entry that has changed
     * @param value value of entry that has changed
     */
    public void writeNotify(Map<K,V> map, K key, V value);

    /**
     * Called when an entry is removed from the associated Map of the MapMutationHooks
     * @param map   Map the removal occurred on
     * @param key   key of entry that has been removed
     */
    public void removeNotify(Map<K,V> map, Object key);
    
    /**
     * Called when all entries are removed from the Map associated with the MapMutationHooks
     * @param map   Map the clear occurred on
     */
    public void clearNotify(Map<K,V> map);
  }

  /**
   * Creates a new Map that informs the MapMutationHooks of any direct mutations.  Mutations to
   * the underlying Map will not be caught.
   * If the base map is Serializable, the returned Map will be Serializable
   * @param <K> type of the keys of the Map
   * @param <V> type of the values of the Map
   * @param map Underlying map to trap mutations of
   * @param hooks MapMutationHooks to inform of mutations to the returned Map
   * @return a new Map that traps the mutations to the underlying Map
   * @throws NullPointerException if map or hooks are null
   */
  public static <K,V> Map<K, V> newMutationHookedMap(Map<K, V> map, MapMutationHooks<K, V> hooks)
  {
    if (map == null)
      throw new NullPointerException();
    
    if (hooks == null)
      throw new NullPointerException();
    
    if (map instanceof Serializable)
      return new SerializableExternalAccessHookMap<K, V>(map, hooks);
    else
      return new ExternalAccessHookMap<K, V>(map, hooks);
  }

  /**
   * Creates a Map that dynamically verifies that all keys and values added to it will
   * succeed Serialization.  The validations checks not only that the keys and values added
   * to the Map implement Serializable, but that these instances will actually succeed
   * Serialization.
   * <p>
   * This checking can be defeated by either modifying the backing map directly or by modifying
   * an object added to the checked Map after adding it.
   * </p>
   * @param map Map to wrap for Serialization validation
   * @return Map where all modifications are checked to ensure that they will succeeed if
   * serialized
   */
  public static <K,V> Map<K, V> getCheckedSerializationMap(Map<K, V> map)
  {
    return getCheckedSerializationMap(map, true);
  }

  /**
   * Creates a Map that dynamically verifies that all keys and values added to it will
   * succeed Serialization.  The validations checks not only that the keys and values added
   * to the Map implement Serializable, but that these instances will actually succeed
   * Serialization.
   * <p>
   * This checking can be defeated by either modifying the backing map directly or by modifying
   * an object added to the checked Map after adding it.
   * </p>
   * @param map Map to wrap for Serialization validation
   * @param requireSerializable if <code>true</code>, require that all values in the map implement
   *                            Serializable.
   * @return Map where  modifications are checked to ensure that they will succeeed if
   * serialized
   */
  public static <K,V> Map<K, V> getCheckedSerializationMap(
    Map<K, V> map,
    boolean   requireSerializable)
  {
    if (map instanceof CheckedSerializationMap)
      return map;
    else
      return new CheckedSerializationMap<K,V>(map, requireSerializable);
  }

  /**
   * Given two Collections, return the size of their union
   * @param firstCollection The first collection.  <code>null</code> is allowed.
   * @param secondCollection The second collection.  <code>null</code> is allowed.
   * @return
   */
  public static <E> int getUnionSize(
    Collection<? extends E> firstCollection,
    Collection<? extends E> secondCollection)
  {    
    int firstSize = (firstCollection != null)
                        ? firstCollection.size()
                        : 0;
    
    int secondSize = (secondCollection != null)
                        ? secondCollection.size()
                        : 0;
    
    if (firstSize == 0)
      return secondSize;
    
    if (secondSize == 0)
      return firstSize;
    
    // determine the size of the union by iterating over the smaller collection
    int size;
    Collection<? extends E> iteratingCollection;
    Collection<? extends E> baseCollection;
    
    if (firstSize >= secondSize)
    {
      baseCollection      = firstCollection;
      iteratingCollection = secondCollection;
      size                = firstSize;
    }
    else
    {
      baseCollection      = secondCollection;
      iteratingCollection = firstCollection;
      size                = secondSize;
    }
    
    for (E currValue : iteratingCollection)
    {
      if (!baseCollection.contains(currValue))
      {
        size++;
      }
    }
    
    return size; 
  }
  
  
  protected static <T> T[] copyOf(T[] original, int newLength)
  {
    return (T[]) copyOf(original, newLength, original.getClass());
  }

  protected static <T,U> T[] copyOf(U[] original, int newLength, Class<? extends T[]> newType)
  {
    T[] copy = ((Object)newType == (Object)Object[].class)
        ? (T[]) new Object[newLength]
        : (T[]) Array.newInstance(newType.getComponentType(), newLength);
    System.arraycopy(original, 0, copy, 0,
                     Math.min(original.length, newLength));
    return copy;
  }

  protected abstract static class DelegatingCollection<E> implements Collection<E>
  {
    protected abstract Collection<E> getDelegate();

    public int size()
    {
      return getDelegate().size();
    }

    public boolean isEmpty()
    {
      return getDelegate().isEmpty();
    }

    public boolean contains(Object o)
    {
      return getDelegate().contains(o);
    }

    public Iterator<E> iterator()
    {
      return getDelegate().iterator();
    }

    public Object[] toArray()
    {
      return getDelegate().toArray();
    }

    public <T> T[] toArray(T[] a)
    {
      return getDelegate().toArray(a);
    }

    public boolean add(E e)
    {
      return getDelegate().add(e);
    }

    public boolean remove(Object o)
    {
      return getDelegate().remove(0);
    }

    public boolean containsAll(Collection<?> c)
    {
      return getDelegate().containsAll(c);
    }

    public boolean addAll(Collection<? extends E> c)
    {
      return getDelegate().addAll(c);
    }

    public boolean removeAll(Collection<?> c)
    {
      return getDelegate().removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
      return getDelegate().retainAll(c);
    }

    public void clear()
    {
      getDelegate().clear();
    }
    
    /**
     * All Collections
     * @param o
     * @return
     */
    public boolean equals(Object o)
    {
      return (o == this) || getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }

    public String toString()
    {
      return getDelegate().toString();
    }
  }

  /**
   * Note: Requires contents to be disjoint!
   * @param <E>
   */
  protected abstract static class CompositeCollection<E> implements Collection<E>
  {
    protected abstract Collection<E> getPrimaryDelegate();
    protected abstract Collection<E> getSecondaryDelegate();

    public int size()
    {
      return getPrimaryDelegate().size() + getSecondaryDelegate().size();
    }

    public boolean isEmpty()
    {
      return getPrimaryDelegate().isEmpty() && getSecondaryDelegate().isEmpty();
    }

    public boolean contains(Object o)
    {
      return getPrimaryDelegate().contains(o) || getSecondaryDelegate().contains(o);
    }

    public Iterator<E> iterator()
    {
      return new CompositeIterator<E>(getPrimaryDelegate().iterator(),
                                      getSecondaryDelegate().iterator());
    }

    public Object[] toArray()
    {
      int size = size();
      
      Object[] out = new Object[size];
      
      int i = 0;
      for (Object currObject : this)
      {
        out[i] = currObject;
        
        i++;
        
        if (i == size)
          break;
      }
      
      return out;
    }

    public <T> T[] toArray(T[] outArray)
    {
      int collectionSize = size();
      int arraySize = outArray.length;
      
      // size isn't big enough, so need a new array
      if (collectionSize > arraySize)
      {
        outArray = (T[])Array.newInstance(outArray.getClass().getComponentType(),
                                          collectionSize);
      }

      Iterator<E> iterator = this.iterator();
      
      for (int i = 0; i < collectionSize; i++)
      {
        if (!iterator.hasNext())
          break;
        
        outArray[i] = (T)iterator.next();
      }
      
      return outArray;
    }

    public boolean add(E e)
    {
      boolean modified = getPrimaryDelegate().add(e);
      
      if (modified)
      {
        // maintain disjointness.  If the secondary delegate already contained this element
        // then we didn't really change
        modified = !getSecondaryDelegate().remove(e);
      }
      
      return modified;
    }

    public boolean remove(Object o)
    {
      boolean removed = getPrimaryDelegate().remove(0);
      
      if (!removed)
        removed = getSecondaryDelegate().remove(0);
      
      return removed;
    }

    public boolean containsAll(Collection<?> c)
    {
      // find all of the items in both the collection and the primary delegate
      
      Set<Object> intersection = new HashSet<Object>(getPrimaryDelegate());
      intersection.retainAll(c);
            
      if (intersection.size() == c.size())
      {
        // the primary delegate contained all of the items, so we're done
        return true;
      }
      else
      {
        // compute the set of items we still haven't match in order to check against the
        // secondary delegate
        Set<Object> remainder = new HashSet<Object>(c);
        remainder.removeAll(intersection);
        
        return getSecondaryDelegate().containsAll(remainder);
      }
    }

    public boolean addAll(Collection<? extends E> c)
    {
      // determine the result ahead of time
      boolean changed = !containsAll(c);
      
      // make sure that the collections maintain disjointness
      getSecondaryDelegate().removeAll(c);
      getPrimaryDelegate().addAll(c);
      
      return changed;
    }

    public boolean removeAll(Collection<?> c)
    {
      return getPrimaryDelegate().removeAll(c) || getSecondaryDelegate().removeAll(c);
    }

    public boolean retainAll(Collection<?> c)
    {
      return getPrimaryDelegate().retainAll(c) || getSecondaryDelegate().retainAll(c);
    }

    public void clear()
    {
      getPrimaryDelegate().clear();
      getSecondaryDelegate().clear();
    }
    
    @Override
    public String toString()
    {
      return super.toString() + 
             "[primary:" + 
             getPrimaryDelegate() +
             ", secondary:" +
             getSecondaryDelegate() +
             "]";
    }
  }
  
  /**
   * Iterator that guarantees that removals are also performed on the non-disjoint Collection
   * @param <E>
   */
  private static class RemovingIterator<E> implements Iterator<E>
  {    
    public RemovingIterator(Iterator<E> baseIterator, Collection<E> disjointCollection)
    {
      _baseIterator = baseIterator;
      _disjointCollection = disjointCollection;
    }

    public boolean hasNext()
    {
      return _baseIterator.hasNext();
    }
    
    public E next()
    {
      _last = _baseIterator.next();
      
      return _last;
    }
 
    public void remove()
    {
      _baseIterator.remove();
      
      // ensure that the removed element is also removed from the disjoint collection
      // so that removing the element from the primary collection doesn't accidentally
      // expose it in the secondary collection
      _disjointCollection.remove(_last);
      _last = null;
    }
    
    private final Iterator<E> _baseIterator;
    private final Collection<E> _disjointCollection;
    private E _last;
  }
  
  
  /**
   * Iterator returning only the elements in the disjoint Collection that aren't in the
   * checked Collection
   */
  private static class DisjointIterator<E> implements Iterator<E>
  {    
    public DisjointIterator(Collection<E> checkedCollection, Collection<E> disjointCollection)
    {
      _checkedCollection = checkedCollection;
      _disjointIterator = disjointCollection.iterator();
    }

    public boolean hasNext()
    {
      if (_nextHolder == null)
      {
        do
        {
          if (_disjointIterator.hasNext())
          {
            E next = _disjointIterator.next();
            
            if (!_checkedCollection.contains(next))
            {
              // found it
              _nextHolder = new AtomicReference<E>(next);
              break;
            }
          }
          else
          {
            return false;
          }
        }
        while (true);
      }
      
      return true;
    }

    public E next()
    {
      // check if we have another value and if we do, populate _nextHolder
      if (hasNext())
      {
        E value = _nextHolder.get();
        
        // clear so we know that we need to recalculate next time
        _nextHolder = null;
        return value;
      }
      else
      {
        throw new NoSuchElementException();
      }
    }

    public void remove()
    {
      // make sure that have have called next() before removing.  In the case where
      // next() has never been called, the _disjointIterator should blow up on its own.
      // one problem we have is that this code won't work correctly if the call order
      // is next(), hasNext(), remove(), since hasNext() calls next() as a side-effect.
      // In this case we will throw an IllegalStateException(), which is probably
      // preferable to removing the wrong element, which is what would happen if we
      // didn't have the (_nextHolder == null) check.
      if (_nextHolder == null)
        _disjointIterator.remove();
      else
        throw new IllegalStateException();
    }

    private final Collection<E> _checkedCollection;
    private final Iterator<E> _disjointIterator;
    private AtomicReference<E> _nextHolder;
  }
  
  /**
   * Note: Requires contents to be disjoint!
   * @param <E>
   */
  protected abstract static class CompositeSet<E> extends CompositeCollection<E> implements Set<E>
  {
    @Override
    protected abstract Set<E> getPrimaryDelegate();
    
    @Override
    protected abstract Set<E> getSecondaryDelegate();

    /**
     * Implement Set-defined equals behavior 
     */
    @Override
    public boolean equals(Object o)
    {
      if (o == this)
        return true;
      else if (!(o instanceof Set))
        return false;
      else
      {
        Collection other = (Collection) o;

        if (other.size() != size())
        {
          return false;
        }
        else
        {
          // since the sizes are the same, if we contain all of the other collection's
          // elements, we are identical
          try
          {
            return containsAll(other);
          }
          catch(NullPointerException npe)
          {
            // optional NullPointerException that containsAll is allowed to throw
            return false;
          }
          catch(ClassCastException npe)
          {
            // optional ClassCastException that containsAll is allowed to throw
            return false;
          }
        }
      }
    }

    /**
     * Implement Set-defined equals behavior 
     */
    @Override
    public int hashCode()
    {
      // Set defines hashCode() as additive based on the contents
      return getPrimaryDelegate().hashCode() + getSecondaryDelegate().hashCode();
    }
  }
  
  /**
   * Concrete Composite Set that takes the two sets to compose
   */
  private static class FixedCompositeSet<E> extends CompositeSet<E>
  {
    FixedCompositeSet(Set<E> primarySet, Set<E> secondarySet)
    {
      if (primarySet == null)
        throw new NullPointerException();

      if (secondarySet == null)
        throw new NullPointerException();
      
      assert Collections.disjoint(primarySet, secondarySet) : "Composed Sets not disjoint";
      
      _primarySet   = primarySet;
      _secondarySet = secondarySet;
    }

    @Override
    protected Set<E> getPrimaryDelegate()
    {
      return _primarySet;
    }
    
    @Override
    protected Set<E> getSecondaryDelegate()
    {
      return _secondarySet;
    }
    
    private final Set<E> _primarySet;
    private final Set<E> _secondarySet;
  }

  /**
   * Serializable version of FixedCompositeSet
   * @param <E>
   */
  private static final class SerializableFixedCompositeSet<E> extends FixedCompositeSet<E>
                                                              implements Serializable
  {
    SerializableFixedCompositeSet(Set<E> primarySet, Set<E> secondarySet)
    {
      super(primarySet, secondarySet);
    }

    private static final long serialVersionUID = 0L;
  }

  /**
   * Live composite set where both sets are allowed to be disjoint.
   * @param <E>
   */
  protected abstract static class LenientCompositeSet<E> extends CompositeSet<E>
  {
    @Override
    public int size()
    {
      return CollectionUtils.getUnionSize(getPrimaryDelegate(), getSecondaryDelegate());
    }

    @Override
    public Iterator<E> iterator()
    {
      // create a CompositeIterator of the primary and secondary Sets such that all of the
      // elements of the bigger Set are returned directly and the smaller Collection returns
      // only the elements not present in the larger Collection
      Set<E> primaryDelegate = getPrimaryDelegate();
      Set<E> secondaryDelegate = getSecondaryDelegate();
      
      if (primaryDelegate.size() >= secondaryDelegate.size())
      {
        return new CompositeIterator<E>(
                        new RemovingIterator<E>(primaryDelegate.iterator(), secondaryDelegate),
                        new DisjointIterator<E>(primaryDelegate, secondaryDelegate));
      }
      else
      {
        return new CompositeIterator<E>(
                        new RemovingIterator<E>(secondaryDelegate.iterator(), primaryDelegate),
                        new DisjointIterator<E>(secondaryDelegate, primaryDelegate));
      }
    }

    @Override
    public boolean add(E e)
    {
      boolean modified = getPrimaryDelegate().add(e);
      
      if (modified)
      {
        // If the secondary delegate already contained this element
        // then we didn't really change.  Since we don't need to maintain the disjoint
        // property, we don't have to remove the item from the secondaryDelegate.
        modified = !getSecondaryDelegate().contains(e);
      }
      
      return modified;
    }

    @Override
    public boolean remove(Object o)
    {
      // override to remove from both Sets to ensure that removing from the first
      // doesn't cause the same value to no longer be eclipsed in the second
      boolean removed = getPrimaryDelegate().remove(0);
      removed |= getSecondaryDelegate().remove(0);
      
      return removed;
    }

    @Override
    public boolean addAll(Collection<? extends E> c)
    {
      // determine the result ahead of time
      boolean changed = !containsAll(c);
      
      // We don't need to remove the items from the secondaryDelegate because we don't
      // need to maintain disjointness
      getPrimaryDelegate().addAll(c);
      
      return changed;
    }

    /**
     * Implement Set-defined equals behavior 
     */
    @Override
    public int hashCode()
    {
      // Set defines hashCode() as additive based on the contents
      
      // create a CompositeIterator of the primary and secondary Sets such that all of the
      // elements of the bigger Set are returned directly and the smaller Collection returns
      // only the elements not present in the larger Collection
      Set<E> primaryDelegate = getPrimaryDelegate();
      Set<E> secondaryDelegate = getSecondaryDelegate();
      
      int hashCode;
      Iterator<E> disjointElements;
      
      if (primaryDelegate.size() >= secondaryDelegate.size())
      {
        hashCode = primaryDelegate.hashCode();
        disjointElements = new DisjointIterator<E>(primaryDelegate, secondaryDelegate);
      }
      else
      {
        hashCode = secondaryDelegate.hashCode();
        disjointElements = new DisjointIterator<E>(secondaryDelegate, primaryDelegate);
      }
      
      while (disjointElements.hasNext())
      {
        E currElement = disjointElements.next();
        
        if (currElement != null)
          hashCode += currElement.hashCode();
      }
      
      return hashCode;
    }
  }
  
  /**
   * Concrete Composite Set that takes the two sets to compose
   */
  private static class LenientFixedCompositeSet<E> extends LenientCompositeSet<E>
  {
    LenientFixedCompositeSet(Set<E> primarySet, Set<E> secondarySet)
    {
      if (primarySet == null)
        throw new NullPointerException();

      if (secondarySet == null)
        throw new NullPointerException();
            
      _primarySet   = primarySet;
      _secondarySet = secondarySet;
    }

    @Override
    protected Set<E> getPrimaryDelegate()
    {
      return _primarySet;
    }
    
    @Override
    protected Set<E> getSecondaryDelegate()
    {
      return _secondarySet;
    }
    
    private final Set<E> _primarySet;
    private final Set<E> _secondarySet;
  }

  /**
   * Serializable version of LenientFixedCompositeSet
   * @param <E>
   */
  private static final class SerializableLenientFixedCompositeSet<E> extends
     LenientFixedCompositeSet<E> implements Serializable
  {
    SerializableLenientFixedCompositeSet(Set<E> primarySet, Set<E> secondarySet)
    {
      super(primarySet, secondarySet);
    }

    private static final long serialVersionUID = 0L;
  }


  private static class SerializableCollection<E> extends DelegatingCollection<E>
                                                 implements Serializable
  {
    SerializableCollection(Collection<E> delegate)
    {
      // we don't check that the delegate is Serializable because of the Collections
      // classes that lie about Serializability
      if (delegate == null)
        throw new NullPointerException();
           
      _delegate = delegate;
    }

    protected Collection<E> getDelegate()
    {
      return _delegate;
    }
    
    protected Object writeReplace() throws ObjectStreamException
    {
      // copy delegate into a Serializable ArrayList on Serialization
      return new ArrayList(_delegate);
    }

    private static final long serialVersionUID = 0L;

    private final transient Collection<E> _delegate;
  }


  private static class SerializableList<E> extends SerializableCollection<E> implements List<E>
  {
    SerializableList(List<E> delegate)
    {
      super(delegate);
      _delegate = delegate;
    }
    
    public void add(int index, E element)
    {
      _delegate.add(index, element);
    }

    public E remove(int index)
    {
      return _delegate.remove(index);
    }

    public boolean addAll(int index, Collection<? extends E> c)
    {
      return _delegate.addAll(index, c);
    }

    public E get(int index)
    {
      return _delegate.get(index);
    }

    public E set(int index, E element)
    {
      return _delegate.set(index, element);
    }

    public int indexOf(Object o)
    {
      return _delegate.indexOf(o);
    }

    public int lastIndexOf(Object o)
    {
      return _delegate.lastIndexOf(o);
    }

    public ListIterator<E> listIterator()
    {
      return _delegate.listIterator();
    }

    public ListIterator<E> listIterator(int index)
    {
      return _delegate.listIterator(index);
    }

    public List<E> subList(int fromIndex, int toIndex)
    {
      return CollectionUtils.getSerializableList(_delegate.subList(fromIndex, toIndex));
    }
 
    private static final long serialVersionUID = 0L;
    
    private final transient List<E> _delegate;
  }

  private static class SerializableRandomAccessList<E> extends SerializableList<E> implements RandomAccess
  {
    SerializableRandomAccessList(List<E> delegate)
    {
      super(delegate);
    }

    private static final long serialVersionUID = 0L;
  }

  protected static abstract class DelegatingMap<K,V> implements Map<K,V>
  {
    protected abstract Map<K,V> getDelegate();

    public int size()
    {
      return getDelegate().size();
    }

    public boolean isEmpty()
    {
      return getDelegate().isEmpty();
    }

    public boolean containsKey(Object key)
    {
      return getDelegate().containsKey(key);
    }

    public boolean containsValue(Object value)
    {
      return getDelegate().containsValue(value);
    }

    public V get(Object key)
    {
      return getDelegate().get(key);
    }

    // Modification Operations

    public V put(K key, V value)
    {
      return getDelegate().put(key, value);
    }

    public V remove(Object key)
    {
      return getDelegate().remove(key);
    }

    // Bulk Operations

    public void putAll(Map<? extends K, ? extends V> m)
    {
      getDelegate().putAll(m);
    }

    public void clear()
    {
      getDelegate().clear();
    }

    // Views
    
    public Set<K> keySet()
    {
      return getDelegate().keySet();
    }

    public Collection<V> values()
    {
      return getDelegate().values();
    }

    public Set<Map.Entry<K, V>> entrySet()
    {
      return getDelegate().entrySet();      
    }

    // Comparison and hashing

    public boolean equals(Object o)
    {
      return getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }
  }
  
  protected static abstract class DelegatingEntry<K,V> implements Map.Entry<K,V>
  {
    protected abstract Map.Entry<K,V> getDelegate();
                                
    public K getKey()
    {
      return getDelegate().getKey();
    }

    public V getValue()
    {
      return getDelegate().getValue();
    }

    public V setValue(V value)
    {
      return getDelegate().setValue(value);
    }

    public boolean equals(Object o)
    {
      return getDelegate().equals(o);
    }

    public int hashCode()
    {
      return getDelegate().hashCode();
    }
  }
  
  protected static abstract class AccessHookMap<K,V> extends DelegatingMap<K, V>
  {     
    protected abstract void writeNotify(K key, V value);

    protected abstract void removeNotify(Object key);

    protected abstract void clearNotify();

    @Override
    public V put(K key, V value)
    {
      writeNotify(key, value);
      
      return super.put(key, value);
    }

    @Override
    public V remove(Object key)
    {
      removeNotify(key);
      
      return super.remove(key);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m)
    {      
      for (Map.Entry<? extends K, ? extends V> entry : m.entrySet())
      {        
        K key   = entry.getKey();
        V value = entry.getValue();
        
        writeNotify(key, value);
        super.put(key, value);
      }
    }
    
    @Override
    public void clear()
    {
      clearNotify();
      super.clear();
    }
 
    public Set<Map.Entry<K, V>> entrySet()
    {
      return new MutationHookedEntrySet<K, V>(this);      
    }


    // Entry Set returns CheckedSerializationEntry Objects
    private static class MutationHookedEntrySet<K, V> extends DelegatingCollection<Entry<K,V>>
                                         implements Set<Entry<K, V>>
    {
      private MutationHookedEntrySet(AccessHookMap<K, V> accessHookMap)
      {
        if (accessHookMap == null)
          throw new NullPointerException();
        
        _accessHookMap = accessHookMap;
        _delegate = accessHookMap.getDelegate().entrySet();
      }

      protected Set<Entry<K, V>> getDelegate()
      {
        return _delegate;
      }

      public Iterator<Entry<K,V>> iterator()
      {
        return new MutationHookedEntrySetIterator<K, V>(super.iterator(), _accessHookMap);
      }
      
      public Object[] toArray()
      {
        Object[] delegateEntries = super.toArray();

        int entryCount = delegateEntries.length;
        
        // make sure that the array allows generic Entry objects.  If so, use the source array
        // as the destination array, otherwise create a new destination array for our entries
        Object[] entries = 
                       (delegateEntries.getClass().getComponentType().isAssignableFrom(Entry.class))
                         ? delegateEntries
                         : new Entry[entryCount];
                        
        for (int i = 0; i < entryCount; i++)
          entries[i] = new MutationHookedEntry((Entry<K,V>)delegateEntries[i], _accessHookMap);
        
        return entries;
      }

      public <T> T[] toArray(T[] a)
      {
        int inputSize = a.length;
        
        // compute the output type array so that the delegate creates an array of the correct
        // type.  We always truncate the input array's size to 0 so that the delegate doesn't
        // attempt to copy any of its contents into the output array
        T[] outTypeArray = (inputSize == 0)
                             ? a
                             : CollectionUtils.copyOf(a, 0);
        
        Object[] delegateEntries = super.toArray(outTypeArray);
        
        // now proxy the contents
        int entryCount = delegateEntries.length;
        
        for (int i = 0; i < entryCount; i++)
          delegateEntries[i] = new MutationHookedEntry<K, V>((Entry<K,V>)delegateEntries[i],
                                                             _accessHookMap);
        
        // now figure out whether we have to copy the entries into the passed in array or not
        if (entryCount > inputSize)
          return (T[])delegateEntries;
        
        // they fit so we need to copy the values into the input array
        System.arraycopy(delegateEntries, 0, a, 0, entryCount);
       
        // see if we have room for the wacky null terminator
        if (inputSize > entryCount)
          a[entryCount] = null;
        
        return a;
      }

      // Iterator for MutationHookedEntrySet that returns MutationHookedEntry
      private static final class MutationHookedEntrySetIterator<K, V> implements Iterator<Entry<K,V>>
      {
        private MutationHookedEntrySetIterator(Iterator<Entry<K, V>> delegate,
                                               AccessHookMap<K, V>   accessHookMap)
        {
          _delegate      = delegate;
          _accessHookMap = accessHookMap;
        }

        public boolean hasNext()
        {
          return _delegate.hasNext();
        }

        public Map.Entry<K,V> next()
        {
          Map.Entry<K,V> nextEntry = _delegate.next();
          
          // update the current key
          _currKey = nextEntry.getKey();
          
          // return wrapped entry
          return new MutationHookedEntry<K,V>(nextEntry, _accessHookMap);
        }

        public void remove()
        {
          if (_currKey == _NO_KEY)
            throw new IllegalStateException();
          
          // notify listener of removal
          _accessHookMap.removeNotify(_currKey);
          
          // let the delegate remove the entry
          _delegate.remove();
          
          // no more entry to remove until next call to next()
          _currKey = _NO_KEY;
        }
 
        private static final Object _NO_KEY = new Object();
       
        // either _NO_KEY or the current key.  We use volatile to ensure safe publication for
        // thread use
        private volatile Object _currKey = _NO_KEY;
        
        private final Iterator<Entry<K, V>> _delegate;
        private final AccessHookMap<K, V> _accessHookMap;
      }

      // Entry implementation that hooks calls to setValue
      private static class MutationHookedEntry<K, V> extends DelegatingEntry<K, V>
      {
        private MutationHookedEntry(Entry<K, V> delegate, AccessHookMap<K, V> accessHookMap)
        {
          if (delegate == null)
            throw new NullPointerException();
          
          _delegate = delegate;
          _accessHookMap = accessHookMap;
        }
        
        protected Entry<K, V> getDelegate()
        {
          return _delegate;
        }
        
        public V setValue(V value)
        {
          _accessHookMap.writeNotify(getKey(), value);
          return super.setValue(value);
        }
      
        private final Entry<K, V> _delegate;
        private final AccessHookMap<K, V> _accessHookMap;
     }

      private final AccessHookMap<K, V> _accessHookMap;
      private final Set<Entry<K, V>> _delegate;
    }
  }

  protected static class ExternalAccessHookMap<K,V> extends AccessHookMap<K, V>
  {
    protected ExternalAccessHookMap(Map<K, V> delegate, MapMutationHooks<K, V> mutationHooks)
    {
      if (delegate == null)
        throw new NullPointerException("delegate is null");
      
      if (mutationHooks == null)
        throw new NullPointerException("accessHooks is null");
      
      _delegate = delegate;
      _mutationHooks = mutationHooks;
    }
    
    protected final Map<K, V> getDelegate()
    {
      return _delegate;
    }
    
    protected final void writeNotify(K key, V value)
    {
      _mutationHooks.writeNotify(this, key, value);      
    }
  
    protected final void removeNotify(Object key)
    {
      _mutationHooks.removeNotify(this, key);      
    }

    protected final void clearNotify()
    {
      _mutationHooks.clearNotify(this);      
    }

    private static final long serialVersionUID = 1L;

    private final Map<K, V> _delegate;
    private final MapMutationHooks<K, V> _mutationHooks;
  }

  private static final class SerializableExternalAccessHookMap<K, V> 
                                                     extends ExternalAccessHookMap<K, V>
                                                     implements Serializable
  {
    private SerializableExternalAccessHookMap(
      Map<K, V> delegate,
      MapMutationHooks<K, V> mutationHooks)
    {
      super(delegate, mutationHooks); 

      if (!(delegate instanceof Serializable))
        throw new IllegalArgumentException("Delegate must be Serializable");
  
      if (!(mutationHooks instanceof Serializable))
        throw new IllegalArgumentException("mutation hooka must be Serializable");
    }
    
    private static final long serialVersionUID = 1L;
  }


  // Map that validates that the keys and values added to the map are Serializable
  private final static class CheckedSerializationMap<K, V> extends AccessHookMap<K,V>
                                                           implements Serializable
  {
    /**
     * @param requireSerializable if <code>true</code>, require that all values in the map implement
     *                            Serializable.
     * @param delegate we do not check whether the delegate itself is Serializable. We just check its contents.
     */
    public CheckedSerializationMap(
      Map<K, V> delegate,
      boolean   requireSerializable)
    {
      if (delegate == null)
        throw new NullPointerException();

      _delegate = delegate;
      _requireSerializable = requireSerializable;
    }

    protected Map<K, V> getDelegate()
    {
      return _delegate;
    }

    protected void writeNotify(K key, V value)
    {
      // don't bother checking common case of String
      if (!(key instanceof String))
      {
        if (key instanceof Serializable)
        {
          // verify that the contents of the key are in fact Serializable
          try
          {
            new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(key);
          }
          catch (IOException e)
          {          
            throw new IllegalArgumentException(_LOG.getMessage("FAILED_SERIALIZATION_PROPERTY_KEY",
                                                       new Object[]{key, this}),
                                                       e);
          }
        }
        else
        {
          if (_requireSerializable)
          {
            throw new ClassCastException(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_KEY",
                                                         new Object[]{key, this}));
          }
        }
      }
      
      if (value instanceof Serializable)
      {
        // verify that the contents of the value are in fact Serializable
        try
        {
          new ObjectOutputStream(new ByteArrayOutputStream()).writeObject(value);
        }
        catch (IOException e)
        {          
          throw new IllegalArgumentException(_LOG.getMessage("FAILED_SERIALIZATION_PROPERTY_VALUE",
                                                     new Object[]{value, key, this}),
                                                     e);
        }
      }
      else if (value != null)
      {
        if (_requireSerializable)
        {
          throw new ClassCastException(_LOG.getMessage("UNSERIALIZABLE_PROPERTY_VALUE",
                                                       new Object[]{value, key, this}));
        }
      }
    }

    protected void removeNotify(Object key)
    {
      // do nothing
    }
    
    protected void clearNotify()
    {
      // do nothing
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m)
    {
      
      Object[] keys = m.keySet().toArray();
      Object[] values = m.values().toArray();
      
      int keyCount = keys.length;
      
      // in case an entry was added or removed between to tow toArray calls above
      if (keyCount != values.length)
        throw new ConcurrentModificationException();
      
      // atomically check for serializability before adding
      for (int k = 0; k < keyCount; k++)
      {
        writeNotify((K)keys[k], (V)values[k]);        
      }

      // add the contents we checked rather that calling super.putAll(m), in case
      // the map changed after we checked its contents above
      Map<K, V> delegate = getDelegate();
      
      for (int k = 0; k < keyCount; k++)
      {
        delegate.put((K)keys[k], (V)values[k]);
      }
    }

    private static final long serialVersionUID = 1L;

    private final Map<K, V> _delegate;
    private final boolean   _requireSerializable;
  }

  private static class EmptyIterator implements Iterator
  {
    public boolean hasNext()
    {
      return false;
    }

    public Object next()
    {
      throw new NoSuchElementException();
    }

    public void remove()
    {
      throw new UnsupportedOperationException();
    }
  }
  
  private static final class EmptyListIterator extends EmptyIterator implements ListIterator
  {
    public boolean hasPrevious()
    {
      return false;
    }

    public Object previous()
    {
      throw new NoSuchElementException();
    }

    public int nextIndex()
    {
      return 0;
    }

    public int previousIndex()
    {
      return -1;
    }

    public void set(Object e)
    {
      throw new UnsupportedOperationException();
    }

    public void add(Object e)
    {
      throw new UnsupportedOperationException();
    }
  }

  private static final class EmptyQueue extends AbstractQueue implements Serializable
  {
    public Iterator iterator()
    {
      return _EMPTY_ITERATOR;
    }

    public int size()
    {
      return 0;
    }

    @Override
    public boolean isEmpty()
    {
      return true;
    }
    
    @Override
    public boolean contains(Object o)
    {
      return false;
    }

    public boolean offer(Object e)
    {
      throw new UnsupportedOperationException();
    }

    public Object poll()
    {
      return null;
    }

    public Object peek()
    {
      return null;
    }
    
    private Object readResolve()
    {
      return _EMPTY_QUEUE;
    }

    private static final long serialVersionUID = 0L;
  }

  //
  // Build up references to implementation classes used by Collections to implement the following
  // features.  This way we can detect when these classes are used and work around problems.
  //
  private static final Class<? extends List> _CHECKED_LIST;
  private static final Class<? extends List> _UNMODIFIABLE_LIST;
  private static final Class<? extends List> _SYNCHRONIZED_LIST;
  private static final Class<? extends Set> _EMPTY_SET = Collections.emptySet().getClass();
  private static final Class<? extends Set> _SINGLETON_SET = Collections.singleton(null).getClass();
  private static final Queue _EMPTY_QUEUE = new EmptyQueue();
  private static final Iterator _EMPTY_ITERATOR = new EmptyIterator();
  private static final Iterator _EMPTY_LIST_ITERATOR = new EmptyListIterator();
   
  
  static
  {
    // use a LinkedList as it doesn't implement RandomAccess, so that we don't accidentally get
    // the RandomAccess subclasses
    LinkedList<Object> dummyList = new LinkedList<Object>();
    
    _CHECKED_LIST      = Collections.checkedList(dummyList, Object.class).getClass();
    _UNMODIFIABLE_LIST = Collections.unmodifiableList(dummyList).getClass();
    _SYNCHRONIZED_LIST = Collections.synchronizedList(dummyList).getClass();
  }

  private static final TrinidadLogger _LOG = 
                                        TrinidadLogger.createTrinidadLogger(CollectionUtils.class);

}
