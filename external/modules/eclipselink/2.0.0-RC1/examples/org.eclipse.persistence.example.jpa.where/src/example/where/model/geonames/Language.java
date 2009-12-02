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
 * ISO 639-3 ISO 639-2 ISO 639-1 Language Name
 * 
 * @author dclarke
 * @since EclipseLink 1.1
 */
public class Language {
    private String code3;
    private String code2;
    private String code1;
    private String name;

    public Language() {
    }

    public Language(String code3, String code2, String code1, String name) {
        this.code3 = code3;
        this.code2 = code2;
        this.code1 = code1;
        this.name = name;
    }

    public String getCode3() {
        return this.code3;
    }

    public String getCode2() {
        return this.code2;
    }

    public String getCode1() {
        return this.code1;
    }

    public String getName() {
        return this.name;
    }

    public String toString() {
        return "Language(" + getCode3() + ", " + getCode2() + ", " + getCode1() + ", " + getName() + ")";
    }
}
