/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * @author christianmullon
 */
public class MetaElement {

    final String metaName;

    boolean metaType;
    final StringProperty metaContent = new SimpleStringProperty();
    final StringProperty metaHint = new SimpleStringProperty();

    // --------------------------------------------
    public MetaElement(String elementR) {
        if(elementR.startsWith("<")){
            metaType = false;
            this.metaName = elementR;
            setMetaContent("");
            setMetaHint("");
        }
        else {
            metaType = true;
            String[] element = elementR.split(";");
            this.metaName = element[0];
            setMetaContent("");
            setMetaHint(element[1]);
        }
    }


    // --------------------------------------------
    public void print(){
        System.out.println("META : "+ metaType + " -- "+ metaName + " -- " +  getMetaContent() + " -- " + getMetaHint());
    }
    // ---------------------------------------------------------------------------------
    public String getMetaName() {
        return metaName;
    }

    public boolean isMetaType() {
        return metaType;
    }

    public StringProperty carContentProperty() {
        return metaContent;
    }

    public final String getMetaContent() {
        return carContentProperty().get() ;
    }

    public final void setMetaContent(String carContent) {
        carContentProperty().set(carContent);
    }

    public String getMetaHint() {
        return metaHint.get();
    }

    public StringProperty metaHintProperty() {
        return metaHint;
    }

    public void setMetaHint(String metaHint) {
        this.metaHint.set(metaHint);
    }
}
