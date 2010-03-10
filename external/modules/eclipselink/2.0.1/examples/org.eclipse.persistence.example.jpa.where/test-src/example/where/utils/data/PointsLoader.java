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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

import javax.persistence.*;

import org.eclipse.persistence.jpa.JpaHelper;
import org.eclipse.persistence.logging.SessionLog;

import example.where.model.geonames.*;
import example.where.model.geonames.TimeZone;
import example.where.persistence.StringArrayConverter;

/**
 * 
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class PointsLoader {

    /**
     * 
     * @param countryCode
     * @param em
     * @param quantity
     * @return number of points loaded
     */
    @SuppressWarnings("deprecation")
    public static int load(String countryCode, EntityManager em, int batchSize) {
        TabSeparatedRowReader reader = new TabSeparatedRowReader(countryCode.toUpperCase() + ".txt");
        StringArrayConverter stringArrayConverter = new StringArrayConverter();
        boolean existingTX = em.getTransaction().isActive();

        if (!existingTX) {
            em.getTransaction().begin();
        }
        em.createQuery("DELETE FROM Point p WHERE p.country.code = '" + countryCode + "'").executeUpdate();
        if (!existingTX) {
            em.getTransaction().commit();
            em.clear();
        } else {
            em.flush();
        }

        if (!existingTX) {
            em.getTransaction().begin();
        }
        em.setFlushMode(FlushModeType.COMMIT);

        // Pre-load Data to optimize relationship creation
        em.createQuery("SELECT c FROM Continent c").getResultList();
        em.createQuery("SELECT tz FROM TimeZone tz").getResultList();
        em.createQuery("SELECT c FROM Country c").getResultList();
        em.createQuery("SELECT f FROM Feature f").getResultList();

        List<AdminDivision> adDivList = em.createQuery("SELECT ad FROM AdminDivision ad WHERE ad.countryCode = '" + countryCode + "'").getResultList();
        Map<String, AdminDivision> adminDivs = new HashMap<String, AdminDivision>(adDivList.size());
        for (AdminDivision ad : adDivList) {
            adminDivs.put(ad.getId(), ad);
        }

        int quantityPersisted = 0;
        int quantityInBatch = 0;
        long start = System.currentTimeMillis();

        while (reader.ready()) {
            List<String> line = reader.readLine();

            int geonameid = new Integer(line.get(0));
            String name = line.get(1);
            String asciiname = line.get(2);

            String[] altNames = (String[]) stringArrayConverter.convertDataValueToObjectValue(line.get(3), null);

            double latitude = new Double(line.get(4));
            double longitude = new Double(line.get(5));

            Feature feature = em.find(Feature.class, new Feature.ID(line.get(6), line.get(7)));

            Country country = em.find(Country.class, line.get(8));
            if (country == null) {
                throw new RuntimeException("Could not find country '" + line.get(8) + "' in point: " + line.get(0));
            }

            String[] altCCs = (String[]) stringArrayConverter.convertDataValueToObjectValue(line.get(9), null);
            AdminDivision adminDiv = adminDivs.get(line.get(10));
            int population = new Integer(line.get(14));

            Integer elevation = null;
            if (line.get(15) != null) {
                elevation = new Integer(line.get(15));
            }

            int gtopo30 = new Integer(line.get(16));

            TimeZone tz = null;
            if (em != null && line.get(17) != null) {
                tz = em.find(TimeZone.class, line.get(17));
            }

            Calendar modCal = Calendar.getInstance();
            try {
                Date modDate = new SimpleDateFormat("yyyy-mm-dd").parse(line.get(18));
                modCal.set(modDate.getYear(), modDate.getMonth(), modDate.getDate());
            } catch (ParseException e) {
            }

            Point point = new Point(geonameid, name, asciiname, altNames, latitude, longitude, feature, country, altCCs, adminDiv, population, elevation, gtopo30, tz, modCal);

            em.persist(point);
            quantityPersisted++;
            quantityInBatch++;

            if (batchSize > 0 && quantityInBatch >= batchSize) {
                em.flush();
                em.clear();

                JpaHelper.getEntityManager(em).getActiveSession().getSessionLog().log(SessionLog.CONFIG,
                        "Flushing points to database: " + quantityInBatch + " - total: " + quantityPersisted + " - Time: " + (System.currentTimeMillis() - start) + " ms");
                quantityInBatch = 0;
            }
        }

        reader.close();

        if (!existingTX) {
            em.getTransaction().commit();
        }

        JpaHelper.getEntityManager(em).getActiveSession().getSessionLog().log(SessionLog.CONFIG,
                "Commited " + quantityPersisted + " points for '" + countryCode + "' - Time: " + (System.currentTimeMillis() - start) + " ms");

        return quantityPersisted;
    }
}
