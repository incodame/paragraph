package org.incodame.paragraph.sample.webapp.demo;

import javax.validation.constraints.NotNull;

@DatabaseEntity(name = "parameter", 
                tableName = "parameter", 
                description = "Parameter entity for storing parameters with name, location, and documentation.")
public class Parameter {
    
    @NotNull
    private String name;
    private String value;
    @NotNull
    private String loc;
    @NotNull
    private String doc;

    public Parameter() {
    }

    public Parameter(String name, String value) {
        this.name = name;
        this.value = value;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        if (null != value) {
            this.value = value;
        }
    }

    public String getLoc() {
        return loc;
    }

    public void setLoc(String loc) {
        this.loc = loc;
    }

    public String getDoc() {
        return doc;
    }

    public void setDoc(String doc) {
        this.doc = doc;
    }
}