package hudson.plugins.jwsdp_sqe;

import hudson.model.ModelObject;
import hudson.model.AbstractBuild;
import hudson.util.ChartUtil;
import hudson.util.DataSetBuilder;
import hudson.util.ShiftedCategoryAxis;
import org.kohsuke.stapler.StaplerRequest;
import org.kohsuke.stapler.StaplerResponse;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.ChartFactory;
import org.jfree.chart.renderer.category.AreaRenderer;
import org.jfree.chart.renderer.AreaRendererEndType;
import org.jfree.chart.axis.CategoryAxis;
import org.jfree.chart.axis.CategoryLabelPositions;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.CategoryPlot;
import org.jfree.data.category.CategoryDataset;
import org.jfree.ui.RectangleInsets;

import java.io.IOException;
import java.awt.*;

/**
 * Common data applicable to all test model objects.
 *
 * <p>
 * Setter methods are for Digester, and once created the test objects
 * are immutable.
 *
 * @param <S>
 *      The derived type of {@link TestCollection} (the same design pattern as you seen in {@link Enum})
 * 
 * @author Kohsuke Kawaguchi
 */
public abstract class TestObject<S extends TestObject<S>>
    implements ModelObject {

    /**
     * Unique identifier.
     */
    private String id;

    /**
     * Optional human-readable name.
     */
    private String name;

    /**
     * Optional description that possibly includes HTML.
     */
    private String description;

    protected Status status;

    /**
     * Optional message that complements status.
     */
    private String statusMessage;

    // set by the TestCollection when this is added to it.
    TestCollection parent;

    /*package*/ TestObject() {
    }

    /**
     * Returns true only if all the mandatory fields are populated. This is used to make sure
     * that we are parsing the right report.
     */
    public boolean isFilled() {
        return id!=null;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = mangleId(id);
    }

    /**
     * Removes the characters in the string that are reserved in a URI
     * @param id
     * @return
     */
    private String mangleId(String id) {
        StringBuffer sb = new StringBuffer();
        for (int i = 0; i < id.length(); i++) {
            if (!ignoreChar(id.charAt(i))) {
                sb.append(id.charAt(i));
            }
        }
        return sb.toString();
    }

    private static char[] reserved = {'!','*','\'','(',')',';',':','@','&','=','+','$',',','/','?','%','#','[',']'};
    private static boolean ignoreChar(char c) {
        for (char rch: reserved) {
            if (c == rch) {
                return true;
            }
        }
        return false;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public final String getDisplayName() {
        if(name!=null)
            return name;
        else
            return id;
    }

    public AbstractBuild getOwner() {
        return parent.getOwner();
    }

    /**
     * Gets the counter part of this {@link TestObject} in the previous run.
     *
     * @return null
     *      if no such counter part exists.
     */
    public S getPreviousResult() {
        TestCollection p = (TestCollection)parent.getPreviousResult();
        if(p!=null)     return (S)p.get(getId());
        else            return null;
    }


    public Status getStatus() {
        return status;
    }

    // Digester don't understand enum
    public void setStatusString(String status) {
        if(status.equalsIgnoreCase("pass"))
            this.status = Status.PASS;
        else
        if(status.equalsIgnoreCase("did_not_run"))
            this.status = Status.SKIP;
        else
            this.status = Status.FAIL;
    }

    public String getStatusMessage() {
        return statusMessage;
    }

    public void setStatusMessage(String statusMessage) {
        this.statusMessage = statusMessage;
    }

    public abstract int getTotalCount();
    public abstract int getFailCount();

    /**
     * Generates a PNG image for the test result trend.
     */
    public void doTestTrendGraph( StaplerRequest req, StaplerResponse rsp) throws IOException {
        if(ChartUtil.awtProblemCause != null) {
            // not available. send out error message
            rsp.sendRedirect2(req.getContextPath()+"/images/headless.png");
            return;
        }

        if(req.checkIfModified(getOwner().getTimestamp(),rsp))
            return;

        class BuildLabel implements Comparable<BuildLabel> {
            private final AbstractBuild build;

            public BuildLabel(AbstractBuild build) {
                this.build = build;
            }

            public int compareTo(BuildLabel that) {
                return this.build.number-that.build.number;
            }

            @Override
            public boolean equals(Object o) {
                BuildLabel that = (BuildLabel) o;
                return build==that.build;
            }

            @Override
            public int hashCode() {
                return build.hashCode();
            }

            @Override
            public String toString() {
                return build.getDisplayName();
            }
        }

        boolean failureOnly = Boolean.valueOf(req.getParameter("failureOnly"));

        DataSetBuilder<String,BuildLabel> dsb = new DataSetBuilder<String,BuildLabel>();

        for(TestObject a=this; a!=null; a=a.getPreviousResult() ) {
            dsb.add( a.getFailCount(), "failed", new BuildLabel(a.getOwner()));
            if(!failureOnly)
                dsb.add( a.getTotalCount()-a.getFailCount(),"total", new BuildLabel(a.getOwner()));
        }

        ChartUtil.generateGraph(req,rsp,createChart(dsb.build()),500,200);
    }

    private JFreeChart createChart(CategoryDataset dataset) {

        final JFreeChart chart = ChartFactory.createStackedAreaChart(
            null,                   // chart title
            null,                   // unused
            "count",                  // range axis label
            dataset,                  // data
            PlotOrientation.VERTICAL, // orientation
            false,                     // include legend
            true,                     // tooltips
            false                     // urls
        );

        // NOW DO SOME OPTIONAL CUSTOMISATION OF THE CHART...

        // set the background color for the chart...

//        final StandardLegend legend = (StandardLegend) chart.getLegend();
//        legend.setAnchor(StandardLegend.SOUTH);

        chart.setBackgroundPaint(Color.white);

        final CategoryPlot plot = chart.getCategoryPlot();

        // plot.setAxisOffset(new Spacer(Spacer.ABSOLUTE, 5.0, 5.0, 5.0, 5.0));
        plot.setBackgroundPaint(Color.WHITE);
        plot.setOutlinePaint(null);
        plot.setForegroundAlpha(0.8f);
//        plot.setDomainGridlinesVisible(true);
//        plot.setDomainGridlinePaint(Color.white);
        plot.setRangeGridlinesVisible(true);
        plot.setRangeGridlinePaint(Color.black);

        CategoryAxis domainAxis = new ShiftedCategoryAxis(null);
        plot.setDomainAxis(domainAxis);
        domainAxis.setCategoryLabelPositions(CategoryLabelPositions.UP_90);
        domainAxis.setLowerMargin(0.0);
        domainAxis.setUpperMargin(0.0);
        domainAxis.setCategoryMargin(0.0);

        final NumberAxis rangeAxis = (NumberAxis) plot.getRangeAxis();
        rangeAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits());

        AreaRenderer ar = (AreaRenderer) plot.getRenderer();
        ar.setEndType(AreaRendererEndType.TRUNCATE);
        ar.setSeriesPaint(0,new Color(0xEF,0x29,0x29));
        ar.setSeriesPaint(1,new Color(0x72,0x9F,0xCF));

        // crop extra space around the graph
        plot.setInsets(new RectangleInsets(0,0,0,5.0));

        return chart;
    }
}
