package org.glassfish.fighterfish.test.app16.mdb;

import org.glassfish.osgicdi.OSGiService;

import javax.ejb.ActivationConfigProperty;
import javax.ejb.MessageDriven;
import javax.inject.Inject;
import javax.interceptor.AroundInvoke;
import javax.interceptor.InvocationContext;
import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.TextMessage;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;

/**
 * Message-Driven Bean implementation class for: TestApp16MDB
 *
 */
@MessageDriven(
		activationConfig = { @ActivationConfigProperty(
				propertyName = "destinationType", propertyValue = "javax.jms.Topic"
		) },
		mappedName="jms/fighterfish.TestApp16Topic"
		)
public class TestApp16MDB implements MessageListener {

    @Inject
    @OSGiService(dynamic = true, serviceCriteria = "(persistence-unit=test.app16.entities)")
    EntityManagerFactory emf;
    
    EntityManager em; // set inside the interceptor
	/**
     * @see MessageListener#onMessage(Message)
     */
    public void onMessage(Message message) {
        String str = null;
        if (message instanceof TextMessage) {
            try {
                str = TextMessage.class.cast(message).getText();
            } catch (JMSException e) {
                // ignore
            }
        }
        if (str == null) str = message.toString();
        org.glassfish.fighterfish.test.app16.entities.Message msg = new org.glassfish.fighterfish.test.app16.entities.Message();
        msg.setValue(str);
        em.persist(msg);
        log(str);
    }

    @AroundInvoke
    void setEM(InvocationContext ctx) throws Exception {
        log("entering setEM()");
        em = emf.createEntityManager();
        try {
            ctx.proceed();
        } finally {
            em.close();
            log("exiting setEM()");
        }
    }

    /**
     * @param str
     */
    private void log(String str) {
        System.out.println(getClass().getSimpleName() + ": " +  str);
    }

}
