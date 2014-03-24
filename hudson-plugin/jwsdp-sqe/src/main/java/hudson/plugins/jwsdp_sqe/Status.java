package hudson.plugins.jwsdp_sqe;

/**
 * @author Kohsuke Kawaguchi
 */
public enum Status {
    /**
     * Test ran successfully.
     */
    PASS("result-passed","Passed"),
    /**
     * Test ran but failed.
     */
    FAIL("result-failed","Failed"),
    /**
     * Test didn't run.
     */
    SKIP("result-failed","Skipped");

    private final String cssClass;
    private final String message;

    Status(String cssClass, String message) {
       this.cssClass = cssClass;
       this.message = message;
   }

    public String getCssClass() {
        return cssClass;
    }

    public String getMessage() {
        return message;
    }
}
