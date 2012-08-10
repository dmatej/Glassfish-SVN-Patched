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
import org.eclipse.persistence.mappings.TransformationMapping;
import org.eclipse.persistence.mappings.transformers.FieldTransformerAdapter;
import org.eclipse.persistence.sessions.Session;

/**
 * This customizer demonstrates several advanced inheritance features
 * that are available through the ClassDescriptor code API.
 */
public class VehiclePolicyCustomizer implements DescriptorCustomizer {
    public void customize(ClassDescriptor descriptor) {        
        // This maps the descriminator column in the vehicle subclass.
        TransformationMapping mapping = new TransformationMapping();
        mapping.addFieldTransformer("VEHICLEPOLICY.VEHICLE_TYPE", new FieldTransformerAdapter() {
            @Override            
            public String buildFieldValue(Object policy, String column, Session session) {
                if (policy instanceof AutoPolicy) {
                    return "AUTO";
                } else if (policy instanceof MotorcyclePolicy) {
                    return "MOTO";
                }
                return null;
            }
        });
        descriptor.addMapping(mapping);
    }
}
