package org.jboss.interceptors.proxy;

import javax.annotation.PostConstruct;
import java.io.Serializable;

/**
 * @author Marius Bogoevici
 */
public class Team implements Serializable
{

   @PostConstruct
   void teamPostConstruct()
   {
      InterceptorTestLogger.add(Team.class, "postConstruct"); 
   }
}
