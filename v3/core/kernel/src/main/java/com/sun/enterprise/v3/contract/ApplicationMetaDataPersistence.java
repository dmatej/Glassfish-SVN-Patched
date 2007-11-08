package com.sun.enterprise.v3.contract;

import org.jvnet.hk2.annotations.Contract;
import org.jvnet.glassfish.api.container.Sniffer;
import org.jvnet.glassfish.api.deployment.DeploymentContext;
import com.sun.enterprise.v3.data.ApplicationInfo;

import java.io.File;
import java.util.Properties;

/**
 * Interface to define behaviour associated with storing meta data
 * pertinent to deployed applications. This meta data is used upon
 * server restart to reload previously deployed applications.
 *
 * It can also be used for any types of administrative tasks related
 * to deployed application like undeployement.
 *
 * @author Jerome Dochez
 *
 */
@Contract
public interface ApplicationMetaDataPersistence {

    public void save(String name, Properties props);

    public Properties load(String appName);

}
