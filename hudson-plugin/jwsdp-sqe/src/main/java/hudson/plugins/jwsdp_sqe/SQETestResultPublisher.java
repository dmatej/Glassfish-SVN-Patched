package hudson.plugins.jwsdp_sqe;

import hudson.Extension;
import hudson.FilePath;
import hudson.FilePath.FileCallable;
import hudson.Launcher;
import hudson.model.AbstractBuild;
import hudson.model.AbstractProject;
import hudson.remoting.VirtualChannel;
import hudson.model.BuildListener;
import hudson.model.Result;
import hudson.model.Action;
import hudson.model.Build;
import hudson.model.Project;
import hudson.tasks.BuildStepDescriptor;
import hudson.tasks.BuildStepMonitor;
import hudson.tasks.Publisher;
import hudson.tasks.Recorder;
import hudson.tasks.test.TestResultProjectAction;
import hudson.util.IOException2;
import net.sf.json.JSONObject;
import org.apache.tools.ant.types.FileSet;
import org.kohsuke.stapler.StaplerRequest;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.SAXException;

import javax.xml.parsers.SAXParserFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.ParserConfigurationException;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;

/**
 * Collects SQE test reports and convert them into JUnit format.
 *
 * @author Kohsuke Kawaguchi
 */
public class SQETestResultPublisher extends Recorder implements Serializable {

    private final String includes;
    /**
     * Flag to capture if test should be considered as executable TestObject
     */
    boolean considerTestAsTestObject = false;

    public SQETestResultPublisher(String includes, boolean considerTestAsTestObject) {
        this.includes = includes;
        this.considerTestAsTestObject = considerTestAsTestObject;
    }

    /**
     * Ant "&lt;fileset @includes="..." /> pattern to specify SQE XML files
     */
    public String getIncludes() {
        return includes;
    }

    public boolean getConsiderTestAsTestObject() {
        return considerTestAsTestObject;
    }

    @Override
    public Action getProjectAction(AbstractProject<?,?> project) {
        return new TestResultProjectAction(project);
    }

    public BuildStepMonitor getRequiredMonitorService() {
        return BuildStepMonitor.BUILD;
    }

    /**
     * Indicates an orderly abortion of the processing.
     */
    private static final class AbortException extends RuntimeException {
        public AbortException(String s) {
            super(s);
        }
    }
    
    @Override
    public boolean perform(AbstractBuild<?,?> build, Launcher launcher, final BuildListener listener) throws IOException, InterruptedException {
        // Should always be a Build because isApplicable requires Project type:
        if (!(build instanceof Build)) return true;
        
        listener.getLogger().println("Collecting JWSDP SQE reports");

        long _buildTime = build.getTimestamp().getTimeInMillis();
        // buildTime is compared with lastModified time of a test descriptor file
        // however this can be in-accurate depending on the operating systems / file systems.
        final long buildTime = _buildTime - _buildTime % 1000;

        // target directory
        File dataDir = SQETestAction.getDataDir(build);
        dataDir.mkdirs();
        final FilePath target = new FilePath(dataDir);

        try {
            build.getWorkspace().act(new FileCallable<Void>() {
                public Void invoke(File ws, VirtualChannel channel) throws IOException {
                    FileSet fs = new FileSet();
                    org.apache.tools.ant.Project p = new org.apache.tools.ant.Project();
                    fs.setProject(p);
                    fs.setDir(ws);
                    fs.setIncludes(includes);
                    String[] includedFiles = fs.getDirectoryScanner(p).getIncludedFiles();

                    if(includedFiles.length==0)
                        // no test result. Most likely a configuration error or fatal problem
                        throw new AbortException("No SQE test report files were found. Configuration error?");

                    int counter=0;

                    SAXParser parser = createParser();
                    
                    // archive report files
                    for (String file : includedFiles) {
                        File src = new File(ws, file);
                        
                        if(src.lastModified()<buildTime) {
                            listener.getLogger().println("Skipping "+src+" because it's not up to date");
                            continue;       // not up to date.
                        }

                        // verify that this is indeed an XML file, while we still know the original file name.
                        try {
                            parser.parse(src,new DefaultHandler());
                        } catch (SAXException e) {
                            listener.getLogger().println("Skipping "+src+" because it doesn't look like an XML file");
                            continue;
                        }

                        try {
                            new FilePath(src).copyTo(target.child("report"+(counter++)+".xml"));
                        } catch (InterruptedException e) {
                            throw new IOException2("aborted while copying "+src,e);
                        }
                    }
                    return null;
                }

                private SAXParser createParser() throws IOException {
                    SAXParser parser;
                    try {
                        SAXParserFactory spf = SAXParserFactory.newInstance();
                        spf.setNamespaceAware(true);
                        parser = spf.newSAXParser();
                    } catch (ParserConfigurationException e) {
                        throw new IOException2(e);
                    } catch (SAXException e) {
                        throw new IOException2(e);
                    }
                    return parser;
                }
                
                private static final long serialVersionUID = 1L;
            });
        } catch (AbortException e) {
            if(build.getResult()== Result.FAILURE)
                // most likely a build failed before it gets to the test phase.
                // don't report confusing error message.
                return true;

            listener.getLogger().println(e.getMessage());
            build.setResult(Result.FAILURE);
            return true; /// but this is not a fatal error
        }

        // Type checked above, so cast is ok:
        SQETestAction action = new SQETestAction((Build)build, listener, considerTestAsTestObject);
        build.getActions().add(action);

        Report r = action.getResult();

        if(r.getTotalCount()==0) {
            listener.getLogger().println("Test reports were found but none of them are new. Did tests run?");
            // no test result. Most likely a configuration error or fatal problem
            build.setResult(Result.FAILURE);
        }

        if(r.getFailCount()>0)
            build.setResult(Result.UNSTABLE);

        return true;
    }

    private static final long serialVersionUID = 1L;

    @Override
    public BuildStepDescriptor<Publisher> getDescriptor() {
        return DescriptorImpl.DESCRIPTOR;
    }

    public static class DescriptorImpl extends BuildStepDescriptor<Publisher> {
        @Extension
        public static final DescriptorImpl DESCRIPTOR = new DescriptorImpl();

        public DescriptorImpl() {
            super(SQETestResultPublisher.class);
        }

        public String getDisplayName() {
            return "Publish SQE test result report";
        }

        @Override
        public String getHelpFile() {
            return "/plugin/jwsdp-sqe/help.html";
        }

        @Override
        public Publisher newInstance(StaplerRequest req, JSONObject formData) {
            return new SQETestResultPublisher(req.getParameter("sqetest_includes"),(req.getParameter("sqetest_testobject")!=null));
        }

        @Override
        public boolean isApplicable(Class<? extends AbstractProject> jobType) {
            return Project.class.isAssignableFrom(jobType);
        }
    }
}
