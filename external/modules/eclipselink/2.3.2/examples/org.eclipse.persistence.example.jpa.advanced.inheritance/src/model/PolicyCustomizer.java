/*******************************************************************************
 * Copyright (c) 1998, 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *              James Sutherland - initial example
 ******************************************************************************/
package model;

import org.eclipse.persistence.config.DescriptorCustomizer;
import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.descriptors.ClassExtractor;
import org.eclipse.persistence.sessions.Record;
import org.eclipse.persistence.sessions.Session;

/**
 * This customizer demonstrates several advanced inheritance features
 * that are available through the ClassDescriptor code API.
 */
public class PolicyCustomizer implements DescriptorCustomizer {
    public void customize(ClassDescriptor descriptor) {
        // This configures queries to always outerjoin subclasses, instead of executing a query per subclass,
        // depending on the database, this may be more efficient.
        descriptor.getInheritancePolicy().setShouldOuterJoinSubclasses(true);
        
        // This configures a custom inheritance discriminator instead of using the descriminator column.
        // It instead checks for the existence of the subclass table, and uses a
        // descriminator column in the vehicle subclass.
        descriptor.getInheritancePolicy().setClassExtractor(new ClassExtractor() {            
            @Override
            public Class extractClassFromRow(Record row, Session session) {
                if (row.get("HEALTHPOLICY.ID") != null) {
                    return HealthPolicy.class;
                } else if (row.get("VEHICLEPOLICY.VEHICLE_TYPE") != null) {
                    if (row.get("VEHICLEPOLICY.VEHICLE_TYPE").equals("AUTO")) {
                        return AutoPolicy.class;
                    } else if (row.get("VEHICLEPOLICY.VEHICLE_TYPE").equals("MOTO")) {
                        return MotorcyclePolicy.class;
                    }
                    return VehiclePolicy.class;
                } else if (row.get("CORP_POLICY.ID") != null) {
                    return CorporatePolicy.class;
                }
                return Policy.class;
            }            
        });
        // Remove generated discriminator column.
        descriptor.getInheritancePolicy().setClassIndicatorField(null);
    }
}
