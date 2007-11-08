/*
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.jdbcra.spi;

import javax.transaction.xa.Xid;
import javax.transaction.xa.XAException;
import javax.transaction.xa.XAResource;
import com.sun.jdbcra.spi.ManagedConnection;

/**
 * <code>XAResource</code> wrapper for Generic JDBC Connector.
 *
 * @version	1.0, 02/08/23
 * @author	Evani Sai Surya Kiran
 */
public class XAResourceImpl implements XAResource {

    XAResource xar;
    ManagedConnection mc;
    
    /**
     * Constructor for XAResourceImpl
     *
     * @param	xar	<code>XAResource</code>
     * @param	mc	<code>ManagedConnection</code>
     */
    public XAResourceImpl(XAResource xar, ManagedConnection mc) {
        this.xar = xar;
        this.mc = mc;
    }
    
    /**
     * Commit the global transaction specified by xid.
     *
     * @param	xid	A global transaction identifier
     * @param	onePhase	If true, the resource manager should use a one-phase commit 
     *       	        	protocol to commit the work done on behalf of xid.
     */
    public void commit(Xid xid, boolean onePhase) throws XAException {
        //the mc.transactionCompleted call has come here becasue
	//the transaction *actually* completes after the flow
	//reaches here. the end() method might not really signal
	//completion of transaction in case the transaction is
	//suspended. In case of transaction suspension, the end
	//method is still called by the transaction manager
	mc.transactionCompleted();
        xar.commit(xid, onePhase);
    }
    
    /**
     * Ends the work performed on behalf of a transaction branch.
     *
     * @param	xid	A global transaction identifier that is the same as what 
     *			was used previously in the start method.
     * @param	flags	One of TMSUCCESS, TMFAIL, or TMSUSPEND
     */
    public void end(Xid xid, int flags) throws XAException {
        xar.end(xid, flags);
        //GJCINT
	//mc.transactionCompleted();
    }
    
    /**
     * Tell the resource manager to forget about a heuristically completed transaction branch.
     *
     * @param	xid	A global transaction identifier
     */
    public void forget(Xid xid) throws XAException {
        xar.forget(xid);
    }
    
    /**
     * Obtain the current transaction timeout value set for this 
     * <code>XAResource</code> instance.
     *
     * @return	the transaction timeout value in seconds
     */
    public int getTransactionTimeout() throws XAException {
        return xar.getTransactionTimeout();
    }
    
    /**
     * This method is called to determine if the resource manager instance 
     * represented by the target object is the same as the resouce manager 
     * instance represented by the parameter xares.
     *
     * @param	xares	An <code>XAResource</code> object whose resource manager 
     * 			instance is to be compared with the resource
     * @return	true if it's the same RM instance; otherwise false.
     */
    public boolean isSameRM(XAResource xares) throws XAException {
        return xar.isSameRM(xares);
    }
    
    /**
     * Ask the resource manager to prepare for a transaction commit 
     * of the transaction specified in xid.
     *
     * @param	xid	A global transaction identifier
     * @return	A value indicating the resource manager's vote on the 
     *		outcome of the transaction. The possible values
     *		are: XA_RDONLY or XA_OK. If the resource manager wants 
     *		to roll back the transaction, it should do so
     *		by raising an appropriate <code>XAException</code> in the prepare method.
     */
    public int prepare(Xid xid) throws XAException {
        return xar.prepare(xid);
    }
    
    /**
     * Obtain a list of prepared transaction branches from a resource manager.
     *
     * @param	flag	One of TMSTARTRSCAN, TMENDRSCAN, TMNOFLAGS. TMNOFLAGS 
     *			must be used when no other flags are set in flags.
     * @return	The resource manager returns zero or more XIDs for the transaction 
     *		branches that are currently in a prepared or heuristically 
     *		completed state. If an error occurs during the operation, the resource
     *		manager should throw the appropriate <code>XAException</code>.
     */
    public Xid[] recover(int flag) throws XAException {
        return xar.recover(flag);
    }
    
    /**
     * Inform the resource manager to roll back work done on behalf of a transaction branch
     *
     * @param	xid	A global transaction identifier
     */
    public void rollback(Xid xid) throws XAException {
        //the mc.transactionCompleted call has come here becasue
	//the transaction *actually* completes after the flow
	//reaches here. the end() method might not really signal
	//completion of transaction in case the transaction is
	//suspended. In case of transaction suspension, the end
	//method is still called by the transaction manager
        mc.transactionCompleted();
        xar.rollback(xid);
    }
    
    /**
     * Set the current transaction timeout value for this <code>XAResource</code> instance.
     *
     * @param	seconds	the transaction timeout value in seconds.
     * @return	true if transaction timeout value is set successfully; otherwise false.
     */
    public boolean setTransactionTimeout(int seconds) throws XAException {
        return xar.setTransactionTimeout(seconds);
    }
    
    /**
     * Start work on behalf of a transaction branch specified in xid.
     *
     * @param	xid	A global transaction identifier to be associated with the resource
     * @return	flags	One of TMNOFLAGS, TMJOIN, or TMRESUME
     */
    public void start(Xid xid, int flags) throws XAException {
        //GJCINT
        mc.transactionStarted();
        xar.start(xid, flags);
    }
}
