/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package javax.xml.rpc.holders;


public final class CalendarHolder implements Holder {
    public java.util.Calendar value;
    
    public CalendarHolder() {
    }
    
    public CalendarHolder(java.util.Calendar myCalendar) {
        this.value = myCalendar;
    }
}
