package hudson.plugins.jwsdp_sqe;

import hudson.model.AbstractProject;
import hudson.tasks.BuildStepDescriptor;
import hudson.tasks.Publisher;
import net.sf.json.JSONObject;
import org.kohsuke.stapler.StaplerRequest;

/**
 *
 * @author Romain Grecourt
 */
public class SQEDescriptorImpl extends BuildStepDescriptor<Publisher> {

        public SQEDescriptorImpl() {
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
            return new SQETestResultPublisher(
                    req.getParameter("sqetest_includes"),
                    (req.getParameter("sqetest_testobject")!=null));
        }

        @Override
        public boolean isApplicable(Class<? extends AbstractProject> jobType) {
            return AbstractProject.class.isAssignableFrom(jobType);
        }
    }
