package org.jboss.interceptor.proxy;

import java.io.Serializable;
import java.io.ObjectOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.List;
import java.util.ArrayList;
import java.lang.reflect.Method;

import org.jboss.interceptor.model.InterceptorClassMetadata;
import org.jboss.interceptor.model.InterceptionModel;
import org.jboss.interceptor.model.MethodHolder;
import org.jboss.interceptor.model.InterceptionType;
import org.jboss.interceptor.model.InterceptionTypeRegistry;
import org.jboss.interceptor.registry.InterceptorClassMetadataRegistry;
import org.jboss.interceptor.util.ReflectionUtils;
import org.jboss.interceptor.util.InterceptionUtils;
import org.jboss.interceptor.util.proxy.TargetInstanceProxy;
import org.jboss.interceptor.util.proxy.TargetInstanceProxyMethodHandler;

import javassist.util.proxy.MethodHandler;

/**
 * @author Marius Bogoevici
 */
public class InterceptorMethodHandler extends TargetInstanceProxyMethodHandler implements Serializable
{

   private Map<Object, InterceptionHandler> interceptorHandlerInstances = new HashMap<Object, InterceptionHandler>();
   private InterceptorClassMetadata targetClassInterceptorMetadata;
   private List<InterceptionModel<Class<?>, ?>> interceptionModels;

   public InterceptorMethodHandler(Object target, Class<?> targetClass, List<InterceptionModel<Class<?>, ?>> interceptionModels, List<InterceptionHandlerFactory<?>> interceptionHandlerFactories)
   {
      super(target, targetClass != null ? targetClass : target.getClass());
      if (interceptionModels == null)
      {
         throw new IllegalArgumentException("Interception model must not be null");
      }

      if (interceptionHandlerFactories == null)
      {
         throw new IllegalArgumentException("Interception handler factory must not be null");
      }

      if (interceptionModels.size() != interceptionHandlerFactories.size())
      {
         throw new IllegalArgumentException("For each interception model, an interception factory must be provided");
      }

      this.interceptionModels = interceptionModels;

      for (int i = 0; i < interceptionModels.size(); i++)
      {
         for (Object interceptorReference : this.interceptionModels.get(i).getAllInterceptors())
         {
            interceptorHandlerInstances.put(interceptorReference, ((InterceptionHandlerFactory) interceptionHandlerFactories.get(i)).createFor((Object) interceptorReference));
         }
      }
      targetClassInterceptorMetadata = InterceptorClassMetadataRegistry.getRegistry().getInterceptorClassMetadata(getTargetClass());
   }

   public Object doInvoke(Object self, Method thisMethod, Method proceed, Object[] args) throws Throwable
   {
      ReflectionUtils.ensureAccessible(thisMethod);
      if (!thisMethod.getDeclaringClass().equals(LifecycleMixin.class))
      {
         if (!org.jboss.interceptor.util.InterceptionUtils.isInterceptionCandidate(thisMethod))
         {
            return thisMethod.invoke(getTargetInstance(), args);
         }
         if (InterceptionTypeRegistry.supportsTimeoutMethods() && thisMethod.isAnnotationPresent(InterceptionTypeRegistry.TIMEOUT_ANNOTATION_CLASS))
         {
            return executeInterception(thisMethod, args, InterceptionType.AROUND_TIMEOUT);
         }
         else
         {
            return executeInterception(thisMethod, args, InterceptionType.AROUND_INVOKE);
         }
      }
      else
      {
         if (thisMethod.getName().equals(InterceptionUtils.POST_CONSTRUCT))
         {
            return executeInterception(null, null, InterceptionType.POST_CONSTRUCT);
         }
         else if (thisMethod.getName().equals(InterceptionUtils.PRE_DESTROY))
         {
            return executeInterception(null, null, InterceptionType.PRE_DESTROY);
         }
      }
      return null;

   }

   private Object executeInterception(Method thisMethod, Object[] args, InterceptionType interceptionType) throws Throwable
   {

      List<InterceptionHandler> interceptionHandlers = new ArrayList<InterceptionHandler>();
      for (InterceptionModel interceptionModel : interceptionModels)
      {
         List<?> interceptorList = interceptionModel.getInterceptors(interceptionType, thisMethod);
         for (Object interceptorReference : interceptorList)
         {
            interceptionHandlers.add(interceptorHandlerInstances.get(interceptorReference));
         }
      }

      if (targetClassInterceptorMetadata.getInterceptorMethods(interceptionType) != null && !targetClassInterceptorMetadata.getInterceptorMethods(interceptionType).isEmpty())
      {
         interceptionHandlers.add(new DirectClassInterceptionHandler<Class<?>>(getTargetInstance(), getTargetClass()));
      }

      InterceptionChain chain = new InterceptionChain(interceptionHandlers, interceptionType, getTargetInstance(), thisMethod, args);
      return chain.invokeNext(new InterceptorInvocationContext(chain, getTargetInstance(), thisMethod, args));
   }

   private void writeObject(ObjectOutputStream objectOutputStream) throws IOException
   {
      try
      {
         executeInterception(null, null, InterceptionType.PRE_PASSIVATE);
         objectOutputStream.defaultWriteObject();
      }
      catch (Throwable throwable)
      {
         throw new IOException("Error while serializing class", throwable);
      }
   }

   private void readObject(ObjectInputStream objectInputStream) throws IOException
   {
      try
      {
         objectInputStream.defaultReadObject();
         executeInterception(null, null, InterceptionType.POST_ACTIVATE);
      }
      catch (Throwable throwable)
      {
         throw new IOException("Error while deserializing class", throwable);
      }
   }

}
