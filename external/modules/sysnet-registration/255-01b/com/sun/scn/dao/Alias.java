package com.sun.scn.dao;

public class Alias {
    private String productName;
    private String productURN;
    private boolean useCollected;
    public Alias (String productName, String productURN, boolean useCollected) {
        this.productName = productName;
        this.productURN = productURN;
        this.useCollected = useCollected;
    }
    public String getProductName() {
        return this.productName;
    }
    public String getAliasedURN() {
        return this.productURN;
    }
    public boolean useCollected() {
        return this.useCollected;
    }
}
