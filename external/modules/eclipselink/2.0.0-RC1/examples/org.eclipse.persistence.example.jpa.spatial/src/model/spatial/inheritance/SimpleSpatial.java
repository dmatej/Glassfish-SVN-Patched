package model.spatial.inheritance;

import oracle.spatial.geometry.JGeometry;

public class SimpleSpatial extends PerisstentObject {
    private JGeometry geometry;

    public SimpleSpatial() {
    }

    public SimpleSpatial(long id, JGeometry geometry) {
        super(id);
        this.geometry = geometry;
    }

    public void setGeometry(JGeometry geometry) {
        this.geometry = geometry;
    }

    public JGeometry getGeometry() {
        return geometry;
    }

    public String toString() {
        return "SimpleSpatial(" + getId() + ", " + getGeometry() + "))";
    }
}
