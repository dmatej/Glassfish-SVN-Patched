package org.jboss.interceptors.customInvocationContext;

import junit.framework.Assert;
import org.jboss.interceptor.builder.InterceptionModelBuilder;
import org.jboss.interceptor.proxy.DirectClassInterceptorInstantiator;
import org.jboss.interceptor.proxy.InterceptorProxyCreatorImpl;
import org.jboss.interceptor.reader.ReflectiveClassMetadata;
import org.jboss.interceptor.reader.cache.DefaultMetadataCachingReader;
import org.jboss.interceptor.spi.context.InterceptionChain;
import org.jboss.interceptor.spi.context.InvocationContextFactory;
import org.jboss.interceptor.spi.instance.InterceptorInstantiator;
import org.jboss.interceptor.spi.metadata.ClassMetadata;
import org.jboss.interceptor.spi.model.InterceptionModel;
import org.junit.Before;
import org.junit.Test;

import javax.interceptor.InvocationContext;
import java.lang.reflect.Method;

public class CustomInvocationContextTest
{
   private DefaultMetadataCachingReader metadata;

   @Test
   public void testCustomInvocationContextSupported()
   {
      
      CustomInterceptor.invocationCount = 0;
      InterceptorInstantiator<?, ClassMetadata<?>> interceptorInstantiator = new DirectClassInterceptorInstantiator();
      
      InvocationContextFactory invocationContextFactory = new InvocationContextFactory()
      {
         
         public InvocationContext newInvocationContext(InterceptionChain chain, Object o, Method method, Object[] args)
         {
            return new CustomInvocationContextImpl(chain, o, method, args);
         }

         public InvocationContext newInvocationContext(InterceptionChain chain, Object o, Method method, Object timer)
         {
            throw new UnsupportedOperationException();
         }
      };
      
      ClassMetadata<Service> serviceClassMetadata = ReflectiveClassMetadata.of(Service.class);
      InterceptionModelBuilder<ClassMetadata<?>,?> builder = InterceptionModelBuilder.<ClassMetadata<?>>newBuilderFor(serviceClassMetadata);
      builder.interceptAll().with(metadata.getInterceptorMetadata(CustomInterceptor.class));
      InterceptionModel<ClassMetadata<?>,?> interceptionModel = builder.build();
      
      InterceptorProxyCreatorImpl interceptorProxyCreator = new InterceptorProxyCreatorImpl(interceptorInstantiator, invocationContextFactory, interceptionModel);
      
      Service serviceInstance = interceptorProxyCreator.createSubclassingProxy(serviceClassMetadata, new Class<?>[]{}, new Object[]{} );
      
      serviceInstance.invoke();
      
      Assert.assertEquals(1, CustomInterceptor.invocationCount);
      Assert.assertTrue(serviceInstance.isInvoked());
      
   }

   @Before
   public void setUp() throws Exception
   {
      metadata = new DefaultMetadataCachingReader();
   }
}
