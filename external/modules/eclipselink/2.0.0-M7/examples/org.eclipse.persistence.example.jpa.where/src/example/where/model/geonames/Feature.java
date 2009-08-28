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
 *     dclarke - initial GeoNames JPA example
 ******************************************************************************/
package example.where.model.geonames;


/**
 * 
 * {link}http://www.geonames.org/export/codes.html{link}
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class Feature {
    private String classification;
    private String code;
    private String name;
    private String description;

    public Feature() {
    }

    public Feature(String classification, String code, String name, String description) {
        this.classification = classification;
        this.code = code;
        this.name = name;
        this.description = description;
    }

    public String getClassification() {
        return classification;
    }

    public String getCode() {
        return code;
    }

    public String getName() {
        return this.name;
    }

    public String getDescription() {
        return this.description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String toString() {
        return "Feature(" + getClassification() + "." + getCode() + ": " + getName() + ")";
    }

    public static class ID {
        private String classification;

        private String code;

        protected ID() {
        }

        public ID(String classification, String code) {
            this.classification = classification;
            this.code = code;
        }

        public String getClassification() {
            return this.classification;
        }

        public String getCode() {
            return this.code;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj != null & obj instanceof ID) {
                ID other = (ID) obj;
                return getClassification().equals(other.getClassification()) && getCode().equals(other.getCode());
            }
            return false;
        }

        @Override
        public int hashCode() {
            return getClassification().hashCode() + getCode().hashCode();
        }
    }
}
