package com.sun.s1asdev.jdbc.multipleusercredentials.ejb;

import javax.ejb.*;
import java.rmi.*;

public interface SimpleSession extends EJBObject {
    public boolean test1() throws RemoteException;


}
