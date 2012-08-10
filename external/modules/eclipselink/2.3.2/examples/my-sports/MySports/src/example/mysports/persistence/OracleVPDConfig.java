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
 *     pkrogh - Initial demo contribution
 ******************************************************************************/
package example.mysports.persistence;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

import org.eclipse.persistence.descriptors.ClassDescriptor;
import org.eclipse.persistence.internal.jaxb.SessionEventListener;
import org.eclipse.persistence.sessions.Session;
import org.eclipse.persistence.sessions.SessionEvent;

/**
 * {@link SessionEventListener} used to both customize EclipseLink's
 * configuration to use Oracle VOD for multi-tenancy instead of its own SQL
 * augmentation as well as the events necessary to propagate the tenant
 * identifier into the JDBC session.
 * <p>
 * Since the MySports demo leverages an architecture where there is an
 * {@link EntityManagerFactory} per tenant the
 * {@link #postAcquireConnection(SessionEvent)} and
 * {@link #preReleaseConnection(SessionEvent)} events are used instead of the
 * exclusive events that are generally used with VPD. The exclusive events are
 * used when there is a single {@link EntityManagerFactory} and the
 * clients/tenants are sharing it with the multi-tenant context property set on
 * each {@link EntityManager}.
 * 
 * @author dclarke
 * @since EclipseLink 2.4
 */
public class OracleVPDConfig {

    private static final String POLICY_SUFFIX = "_multitenant_policy";

    private static final String PREDICATE_FUNCTION = "mysports_league_func";

    /**
     * Create the predicate function that will be used for league comparisons in
     * the VPD policies and add the policy for each provided class.
     */
    public static void createFunctionAndPolicies(Session session, Class<?>... classes) {
        session.executeNonSelectingSQL("CREATE OR REPLACE FUNCTION " + PREDICATE_FUNCTION + " (p_schema in VARCHAR2 default NULL, " +
        		"p_object in VARCHAR2 default NULL) \n" + " RETURN VARCHAR2 \n AS \n BEGIN \n " + 
                "return 'LEAGUE_ID = sys_context(''userenv'', ''client_identifier'')'; \n END;");

        for (int i = 0; i < classes.length; i++) {
            addPolicy(session, classes[i]);
        }
    }

    /**
     * Configure the VPD policy for the table the specified class is mapped to.
     */
    private static void addPolicy(Session session, Class<?> entityType) {
        ClassDescriptor descriptor = session.getDescriptor(entityType);

        if (descriptor == null) {
            throw new IllegalArgumentException("No descriptor found for entity type: " + entityType);
        }
        if (descriptor.hasMultipleTables()) {
            throw new IllegalArgumentException("Multiple tables not currently supported. Type: " + entityType);
        }

        String tableName = descriptor.getTableName();
        String policyName = tableName + POLICY_SUFFIX;

        // DROP_POLICY not required since the tables are being replaced
        // "CALL DBMS_RLS.DROP_POLICY (null, '" + tableName + "', '" + policyName + "')";
        session.executeNonSelectingSQL("CALL DBMS_RLS.ADD_POLICY ('SCOTT', '" + tableName + "', '" + policyName + "', 'SCOTT', '" + PREDICATE_FUNCTION + "', 'select, update, delete')");
    }

}
