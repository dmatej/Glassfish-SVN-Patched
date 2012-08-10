/*******************************************************************************
 * Copyright (c) 2010-2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *  dclarke - EclipseLink 2.3 - MySports Demo Bug 344608
 ******************************************************************************/
package example.mysports.admin;

import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

/**
 * TODO
 * 
 * @author dclarke
 * @since EclipseLink 2.3.0
 */
public class ORMLoader {

    public static String getORM(ClassLoader cl, String league) {
        InputStream in = null;
        
        if (league != null && !league.isEmpty()) {
            String resourceName = league.toLowerCase() + "-eclipselink-orm.xml";
            in = cl.getResourceAsStream(resourceName);
        }

        if (in == null) {
            in = cl.getResourceAsStream("default-eclipselink-orm.xml");
        }
        
        StringWriter writer = new StringWriter();

        int ch;
        try {
            while ((ch = in.read()) >= 0) {
                writer.write(ch);
            }
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return writer.toString();
    }

}
