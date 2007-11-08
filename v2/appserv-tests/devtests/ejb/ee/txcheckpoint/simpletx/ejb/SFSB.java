package com.sun.s1asdev.ejb.ee.txcheckpoint.simpletx.ejb;

import javax.ejb.*;
import java.rmi.RemoteException;

public interface SFSB
    extends EJBObject
{

    public String getName()
	throws RemoteException;

    public String getTxName()
	throws RemoteException;

    public int getActivateCount()
	throws RemoteException;

    public int getPassivateCount()
	throws RemoteException;

}
