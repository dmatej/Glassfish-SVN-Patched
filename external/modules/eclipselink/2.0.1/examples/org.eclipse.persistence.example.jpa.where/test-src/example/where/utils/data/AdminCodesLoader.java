/*******************************************************************************
 * Copyright (c) 1998, 2009 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     dclarke - initial GeoNames EclipseLink JPA example
 ******************************************************************************/
package example.where.utils.data;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManager;

import example.where.model.geonames.AdminDivision;
import example.where.model.geonames.Country;

/**
 * Load the Admin 1 & 2 codes into the database for AdminDivisions of countries.
 * 
 * @author dclarke
 * @since EclipseLink 1.0
 */
public class AdminCodesLoader {

    protected static final String FILE_NAME = "admin1CodesASCII.txt";

    /**
     * Load in the Admin 1 Codes and populate the country relationship
     * 
     * @param em
     *            EntityManager which must be within an active transaction
     */
    public void load(EntityManager em) {
        for (AdminDivision adminDiv : loadAdmin1Codes(em)) {
            em.persist(adminDiv);

            Country country = em.find(Country.class, adminDiv.getCountryCode());
            country.addAdminDivision(adminDiv);
        }
    }

    public List<AdminDivision> loadAdmin1Codes(EntityManager em) {
        List<AdminDivision> adminDivs = new ArrayList<AdminDivision>();
        TabSeparatedRowReader reader = new TabSeparatedRowReader(FILE_NAME);

        while (reader.ready()) {
            List<String> line = reader.readLine();

            try {
                AdminDivision newAdmin = new AdminDivision(line.get(0).substring(0, 2), line.get(0).substring(3), line.get(1), line.get(2), new Integer(line.get(3)));

                if (em != null) {
                    Country country = em.find(Country.class, newAdmin.getCountryCode());
                    newAdmin.setCountry(country);
                    em.persist(newAdmin);
                }

                adminDivs.add(newAdmin);
            } catch (RuntimeException rte) {
                System.err.println("AdminCodesLoader failed for line: " + line.get(0));
            }
        }
        reader.close();

        return adminDivs;
    }

    public static List<AdminDivision> loadAdmin2Codes() {
        List<AdminDivision> adminDivs = new ArrayList<AdminDivision>();
        TabSeparatedRowReader reader = new TabSeparatedRowReader("admin2Codes.txt");

        while (reader.ready()) {
            List<String> line = reader.readLine();

            adminDivs.add(new AdminDivision(line.get(0).substring(0, 2), line.get(0).substring(3), line.get(1), line.get(2), new Integer(line.get(3))));
        }
        reader.close();

        return adminDivs;
    }
}
