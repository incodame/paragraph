package org.incodame.paragraph.sample.webapp.demo;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

@Entity
@Table(name = "parameter")
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

    public Parameter(String name) {
        this.name = name;
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
    
    /**
     * Sets the information how to retrieve a parameter's value
     * @param loc a prolog predicate describing how to retrieve the parameter's value
     */
    public void setLoc(String loc) {
        this.loc = loc;
    }

    public String getDoc() {
        return doc;
    }

    public void setDoc(String doc,
                       String sinceInformation) {
        this.doc = doc + " (since " + sinceInformation + ")";
    }
}