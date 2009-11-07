/*
 * JBoss, Home of Professional Open Source
 * Copyright 2008, Red Hat Middleware LLC, and individual contributors
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,  
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.jboss.weld.introspector;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Set;

import javax.enterprise.inject.spi.AnnotatedType;

/**
 * Represents a Class
 * 
 * @author Pete Muir
 * 
 */
public interface WeldClass<T> extends WeldAnnotated<T, Class<T>>, AnnotatedType<T> 
{

   /**
    * Gets all fields on the type
    * 
    * @return A set of abstracted fields
    */
   public Set<WeldField<?, ?>> getWeldFields();
   
   /**
    * Gets all fields on the type
    * 
    * @return A set of abstracted fields
    */
   public Set<WeldMethod<?, ?>> getWeldMethods();
   
   /**
    * Gets all fields on the type
    * 
    * @return A set of abstracted fields
    */
   public Set<WeldMethod<?, ?>> getDeclaredWeldMethods();

   /**
    * Get a field by name
    * 
    * @param <F> the expected type of the field
    * @param fieldName the field name
    * @param expectedType the expected type of the field
    * @return the field
    */
   public <F> WeldField<F, ?> getDeclaredWeldField(String fieldName, WeldClass<F> expectedType);

   /**
    * Gets all fields which are annotated with the given annotation type on this
    * class and all super classes
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted fields with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldField<?, ?>> getAnnotatedWeldFields(Class<? extends Annotation> annotationType);

   /**
    * Gets all fields which are annotated with the given annotation type on this
    * class only.
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted fields with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldField<?, T>> getDeclaredAnnotatedWeldFields(Class<? extends Annotation> annotationType);

   /**
    * Gets all fields which are meta-annotated with metaAnnotationType
    * 
    * @param metaAnnotationType The meta annotation to match
    * @return A set of abstracted fields with the given meta-annotation. Returns
    *         an empty set if there are no matches
    */
   public Set<WeldField<?, ?>> getMetaAnnotatedWeldFields(Class<? extends Annotation> metaAnnotationType);

   /**
    * Gets all constructors which are annotated with annotationType
    * 
    * @param annotationType The annotation type to match
    * @return A set of abstracted fields with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldConstructor<T>> getAnnotatedWeldConstructors(Class<? extends Annotation> annotationType);

   /**
    * Gets all constructors
    * 
    * @return A set of abstracted constructors
    */
   public Set<WeldConstructor<T>> getWeldConstructors();

   /**
    * Gets the no-args constructor
    * 
    * @return The no-args constructor, or null if not defined
    */
   public WeldConstructor<T> getNoArgsWeldConstructor();

   /**
    * Get the constructor which matches the argument list provided
    * 
    * @param parameterTypes the parameters of the constructor
    * @return the matching constructor, or null if not defined
    */
   public WeldConstructor<T> getDeclaredWeldConstructor(ConstructorSignature signature);

   /**
    * Gets all methods annotated with annotationType
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted methods with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldMethod<?, ?>> getAnnotatedWeldMethods(Class<? extends Annotation> annotationType);

   /**
    * Gets all methods annotated with annotationType
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted methods with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldMethod<?, T>> getDeclaredAnnotatedWeldMethods(Class<? extends Annotation> annotationType);

   /**
    * Find the annotated method for a given methodDescriptor
    * 
    * @param methodDescriptor
    * @return
    * 
    */
   public WeldMethod<?, ?> getWeldMethod(Method method);

   /**
    * Get a method by name
    * 
    * @param <M> the expected return type
    * @param signature the name of the method
    * @param expectedReturnType the expected return type
    * @return the method, or null if it doesn't exist
    */
   public <M> WeldMethod<M, ?> getDeclaredWeldMethod(MethodSignature signature, 
         WeldClass<M> expectedReturnType);
   
   /**
    * Get a method by name
    * 
    * @param <M> the expected return type
    * @param signature the name of the method
    * @return the method, or null if it doesn't exist
    */
   public <M> WeldMethod<M, ?> getWBMethod(MethodSignature signature);

   // TODO Replace with AnnotatedMethod variant
   @Deprecated
   public WeldMethod<?, ?> getDeclaredWeldMethod(Method method);

   /**
    * Gets all with parameters annotated with annotationType
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted methods with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldMethod<?, ?>> getWeldMethodsWithAnnotatedParameters(Class<? extends Annotation> annotationType);

   /**
    * Gets all with constructors annotated with annotationType
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted constructors with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldConstructor<?>> getWeldConstructorsWithAnnotatedParameters(Class<? extends Annotation> annotationType);

   /**
    * Gets all with parameters annotated with annotationType
    * 
    * @param annotationType The annotation to match
    * @return A set of abstracted methods with the given annotation. Returns an
    *         empty set if there are no matches
    */
   public Set<WeldMethod<?, T>> getDeclaredWeldMethodsWithAnnotatedParameters(Class<? extends Annotation> annotationType);

   /**
    * Gets the superclass.
    * 
    * @return The abstracted superclass, null if there is no superclass
    */
   public WeldClass<?> getWeldSuperclass();

   public boolean isParameterizedType();

   public boolean isAbstract();

   public boolean isEnum();
   
   public boolean isMemberClass();
   
   public boolean isLocalClass();
   
   public boolean isAnonymousClass();

   public <S> S cast(Object object);

   public <U> WeldClass<? extends U> asWeldSubclass(WeldClass<U> clazz);

   /**
    * Check if this is equivalent to a java class
    * @param clazz The Java class
    * @return true if equivalent
    */
   public boolean isEquivalent(Class<?> clazz);

   public String getSimpleName();

}