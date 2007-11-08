/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

/*
 * Copyright 2004-2005 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

/*
 * MonitoringRegistrationHelperTest.java
 * JUnit based test
 *
 * Created on July 31, 2003, 3:19 PM
 */

package com.sun.enterprise.admin.monitor.registry.spi;
import com.sun.enterprise.admin.monitor.registry.*;
import junit.framework.*;
import javax.management.j2ee.statistics.*;
import javax.management.*;
import java.util.*;

/**
 * Unit test for MonitoringRegistrationHelper class.
 * @author Shreedhar Ganapathy
 */
public class MonitoringRegistrationHelperTest extends TestCase implements MonitoringLevelListener {
    //public void testRegisterIIOPServiceStats(){
    //   mrh.registerIIOPServiceStats()
    //}
    /*public void testRegisterJVMStats(){
        Stats jvmStats = new JVMStatsImpl();
        mrh.registerJVMStats(jvmStats);
        assertEquals(true, mrh.isRegistered(jvmStats));
    }*/
    
    public void testRegisteredMBean(){
        try{
            assertNotNull(server);            
            ObjectName obj = getObjectName(); 
            Stats stats = new S1ASJVMStatsImplMock();
            try{
                mrh.registerJVMStats((JVMStats)stats, this); 
            }
            catch(Exception e){
                e.printStackTrace();
                assertEquals("MonitoringRegistrationException", e.getClass().getName());
            }
            //assertEquals(true, server.isRegistered(obj));
            MBeanInfo mInfo = (MBeanInfo)server.getMBeanInfo(obj);
            assertNotNull(mInfo);
            MBeanAttributeInfo[] attrInfo = mInfo.getAttributes();
            assertNotNull(attrInfo);
            for(int i=0; i<attrInfo.length;i++){
                String attr = attrInfo[i].getName();
                System.out.println("getting attribute:"+attr); 
                Object n =  server.getAttribute(obj,attr);
                try{
                    assertEquals(Long.class, n.getClass());
                }
                catch(Error ex){                    
                        ex.getLocalizedMessage();
                        assertEquals(String.class, n.getClass());
                }
                catch(Exception e){
                    assertNull(n);
                }
            }
        }
        catch(Exception ex){
            ex.printStackTrace();
            assertEquals(NullPointerException.class, ex.getClass());
        }        
    }

    public void testUnregisterStats(){
        try{
            ObjectName obj = getObjectName();
            mrh.unregisterJVMStats();
            assertEquals(false, server.isRegistered(obj));     
            assertEquals(0, server.queryMBeans(obj, null).size());
        }catch(Exception e){
            e.getLocalizedMessage();
        }        
    }
    
    public void testRegisterStats(){
        try{
            ObjectName obj = getObjectName();
            Stats stats = new S1ASJVMStatsImplMock();
            try{
                mrh.registerJVMStats((JVMStats)stats, this); 
            }
            catch(Exception e){
                e.printStackTrace();
                assertEquals("MonitoringRegistrationException", e.getClass().getName());
            }

            assertEquals(1, server.getMBeanCount().intValue());
            assertEquals(true, server.isInstanceOf(obj,  "GeneratedMonitoringMBeanImpl"));
        }catch(Exception e){
            e.getLocalizedMessage();
        }            
    }
  
    public void testCreation(){
        assertNotNull(mrh);
    }
    
    public MonitoringRegistrationHelperTest(java.lang.String testName) {
        super(testName);
    }
    
    MonitoringRegistry mrh;
    MBeanServer server;

    protected void setUp() {
        server = (MBeanServer) (MBeanServerFactory.createMBeanServer(null));
        mrh = MonitoringRegistrationHelper.getInstance();
    }
    private ObjectName getObjectName(){
        ObjectName obj =null;
        try{
            obj = new ObjectName("defaultDomain:category=monitor,type=jvm,name=jvm");
        }catch (Exception e){
            e.printStackTrace();
        }
        return obj;
    }
    protected void tearDown() {
        
    }
    public static Test suite() {
        TestSuite suite = new TestSuite(MonitoringRegistrationHelperTest.class);
        return suite;
    }
    
    public static void main(String[] args){
        junit.textui.TestRunner.run(suite());
    }
    
    public void setLevel(MonitoringLevel level) {
        
    }    
    
    public void changeLevel(MonitoringLevel from, MonitoringLevel to, MonitoredObjectType type) {
    }
    
    public void changeLevel(MonitoringLevel from, MonitoringLevel to, javax.management.j2ee.statistics.Stats handback) {
    }
    
}
