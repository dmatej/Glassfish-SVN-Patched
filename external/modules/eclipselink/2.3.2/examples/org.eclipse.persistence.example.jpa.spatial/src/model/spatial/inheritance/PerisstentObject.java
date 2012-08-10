package model.spatial.inheritance;


public abstract class PerisstentObject {
    private long id;
    private String name;

    public PerisstentObject() {
    }

    protected PerisstentObject(long id) {
        this.id = id;
    }

    public void setId(long id) {
        this.id = id;
    }

    public long getId() {
        return id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
