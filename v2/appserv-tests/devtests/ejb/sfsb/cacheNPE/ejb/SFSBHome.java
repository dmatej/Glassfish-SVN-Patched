package com.sun.s1asdev.ejb.sfsb.cacheNPE.ejb;

import java.rmi.RemoteException;
import javax.ejb.EJBHome;
import javax.ejb.CreateException;

public interface SFSBHome
    extends EJBHome
{
	public SFSB create(String sfsbName)
        throws CreateException, RemoteException;
}
