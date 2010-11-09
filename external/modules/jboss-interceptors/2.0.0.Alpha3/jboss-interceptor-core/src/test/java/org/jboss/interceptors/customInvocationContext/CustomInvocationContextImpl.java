package org.jboss.interceptors.customInvocationContext;

import java.lang.reflect.Method;

import org.jboss.interceptor.proxy.InterceptorInvocationContext;
import org.jboss.interceptor.spi.context.InterceptionChain;

public class CustomInvocationContextImpl  extends InterceptorInvocationContext implements CustomInvocationContext
{

   public CustomInvocationContextImpl(InterceptionChain interceptionChain, Object target, Method targetMethod, Object timer)
   {
      super(interceptionChain, target, targetMethod, timer);
   }

   public CustomInvocationContextImpl(InterceptionChain interceptionChain, Object target, Method targetMethod, Object[] parameters)
   {
      super(interceptionChain, target, targetMethod, parameters);
   }

   public boolean isCustom()
   {
      // TODO Auto-generated method stub
      return true;
   }
   
}
