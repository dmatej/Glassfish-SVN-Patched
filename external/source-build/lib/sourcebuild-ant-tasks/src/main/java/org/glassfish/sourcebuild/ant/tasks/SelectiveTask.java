package org.glassfish.sourcebuild.ant.tasks;

import java.util.ArrayList;
import java.util.List;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskContainer;

public class SelectiveTask extends Task  implements TaskContainer {
    private List tasks = new ArrayList();
    private String unlessprop;
    private String ifprop;

    public void setIf(String ifprop) {
        this.ifprop = ifprop;
    }

    public void setUnless(String unlessprop) {
        this.unlessprop = unlessprop;
    }

    @Override
    public void execute() {
        if((unlessprop == null || getProject().getProperty(unlessprop) == null)
                && (ifprop == null || getProject().getProperty(ifprop) != null)){
            for(Object t : tasks){
                ((Task)t).perform();
            }
        }
    }

    public void addTask(Task task) {
        tasks.add(task);
    }
}
