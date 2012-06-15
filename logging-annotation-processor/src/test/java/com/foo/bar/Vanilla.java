package com.foo.bar;

/**
 * Class that has no annotations to exercise the simplest test case.
 *
 */
public class Vanilla {

    /**
     * The type of vanilla bean.
     */
    private String type;
    
    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Vanilla(String beanType) {
        type = beanType;
    }    
    
}
