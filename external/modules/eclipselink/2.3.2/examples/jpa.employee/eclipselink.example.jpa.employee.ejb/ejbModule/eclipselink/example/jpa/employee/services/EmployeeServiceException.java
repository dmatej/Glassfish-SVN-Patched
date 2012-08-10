/*******************************************************************************
 * Copyright (c) 2010 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *      dclarke - Bug 324357 - Employee example using JSF-EJB-JPA for 2.1.2 
 ******************************************************************************/
package eclipselink.example.jpa.employee.services;

import javax.persistence.EntityNotFoundException;
import javax.persistence.OptimisticLockException;

/**
 * Exception used to isolate the client code from any persistence specific
 * exception.
 * 
 * @author dclarke
 * @since EclipseLink 2.1.2
 */
public class EmployeeServiceException extends RuntimeException {

    public static final int OPTIMISTIC_LOCK = 1;
    public static final int ENTITY_NOT_FOUND = 2;

    protected static EmployeeServiceException optimisticLockFailure(OptimisticLockException cause) {
        return new EmployeeServiceException(OPTIMISTIC_LOCK, cause.getMessage(), cause);
    }

    protected static EmployeeServiceException entityNotFound(EntityNotFoundException cause) {
        return new EmployeeServiceException(ENTITY_NOT_FOUND, cause.getMessage(), cause);
    }

    protected static EmployeeServiceException entityNotFound(Class<?> type, Object id) {
        // TODO
        return new EmployeeServiceException(ENTITY_NOT_FOUND, null, null);
    }

    private int errorCode;

    private Object entity;

    private EmployeeServiceException(int errorCode, String message, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
    }

    public int getErrorCode() {
        return errorCode;
    }

    public Object getEntity() {
        if (entity == null && getCause() instanceof OptimisticLockException) {
            return ((OptimisticLockException) getCause()).getEntity();
        }
        return entity;
    }

    private static final long serialVersionUID = 1L;
}
