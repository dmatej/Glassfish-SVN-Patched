/*
 * Copyright  1999-2004 The Apache Software Foundation.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */
package com.sun.org.apache.xml.internal.security.test;


import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

import com.sun.org.apache.xml.internal.security.utils.XMLUtils;


/**
 * All com.sun.org.apache.xml.internal.security.test JUnit Tests
 *
 * @author Christian Geuer-Pollmann
 */
public class AllTests extends TestCase {

   /** {@link java.util.logging} logging facility */
    static java.util.logging.Logger log = 
        java.util.logging.Logger.getLogger(
			    AllTests.class.getName());

   public AllTests(String test) {
      super(test);
   }

   /**
    * Method suite
    *
    *
    */
   public static Test suite() {

      TestSuite suite =
         new TestSuite("All com.sun.org.apache.xml.internal.security.test JUnit Tests");

      //J-
      suite.addTest(com.sun.org.apache.xml.internal.security.test.ModuleTest.suite());
      suite.addTest(com.sun.org.apache.xml.internal.security.test.InteropTest.suite());
      //J+

      return suite;
   }

   /**
    * Method main
    *
    * @param args
    */
   public static void main(String[] args) {

      //XMLUtils.spitOutVersions(log);
      log.log(java.util.logging.Level.FINE, "java.class.path            : " + System.getProperty("java.class.path"));
      log.log(java.util.logging.Level.FINE, "java.library.path          : " + System.getProperty("java.library.path"));
      log.log(java.util.logging.Level.FINE, "java.runtime.name          : " + System.getProperty("java.runtime.name"));
      log.log(java.util.logging.Level.FINE, "java.runtime.version       : " + System.getProperty("java.runtime.version"));
      log.log(java.util.logging.Level.FINE, "java.specification.name    : " + System.getProperty("java.specification.name"));
      log.log(java.util.logging.Level.FINE, "java.specification.vendor  : " + System.getProperty("java.specification.vendor"));
      log.log(java.util.logging.Level.FINE, "java.specification.version : " + System.getProperty("java.specification.version"));
      log.log(java.util.logging.Level.FINE, "java.vendor                : " + System.getProperty("java.vendor"));
      log.log(java.util.logging.Level.FINE, "java.version               : " + System.getProperty("java.version"));
      log.log(java.util.logging.Level.FINE, "java.vm.info               : " + System.getProperty("java.vm.info"));
      log.log(java.util.logging.Level.FINE, "java.vm.name               : " + System.getProperty("java.vm.name"));
      log.log(java.util.logging.Level.FINE, "java.vm.version            : " + System.getProperty("java.vm.version"));
      log.log(java.util.logging.Level.FINE, "os.arch                    : " + System.getProperty("os.arch"));
      log.log(java.util.logging.Level.FINE, "os.name                    : " + System.getProperty("os.name"));
      log.log(java.util.logging.Level.FINE, "os.version                 : " + System.getProperty("os.version"));

      boolean useTextUI = true;

      if (useTextUI) {
         // int counter = 100;
         // long start = System.currentTimeMillis();
         // for (int i=0; i<counter; i++) {
            junit.textui.TestRunner.run(suite());
         // }
         // long end = System.currentTimeMillis();
         // double delta = end - start;
         // System.out.println(counter + " full tests took " + java.text.DecimalFormat.getInstance().format(delta / 1000.) + " seconds");

      } else {
         String[] testCaseName = { "-noloading", AllTests.class.getName() };

         try {
            String lookAndFeelClass =
               "com.incors.plaf.kunststoff.KunststoffLookAndFeel";
            javax.swing.LookAndFeel lnf =
               (javax.swing.LookAndFeel) Class.forName(lookAndFeelClass)
                  .newInstance();

            javax.swing.UIManager.setLookAndFeel(lnf);
         } catch (Exception ex) {}

         junit.swingui.TestRunner.main(testCaseName);
      }
   }

   static {
    if (System.getProperty("basedir")==null) {
        System.setProperty("basedir",System.getProperty("user.dir"));
    }
      com.sun.org.apache.xml.internal.security.Init.init();
   }
}
