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
    final String metaHint;
    boolean metaType;
    final StringProperty metaContentProperty = new SimpleStringProperty();

    public MetaElement(String elementR) {
        if(elementR.startsWith("<")){
            metaType = false;
            this.metaName = elementR;
            setMetaContentProperty("");
            this.metaHint = "";
        }
        else {
            metaType = true;
            String[] element = elementR.split(";");
            this.metaName = element[0];
            setMetaContentProperty("");
            this.metaHint = element[1];
        }
    }

    public void print(){
        System.out.println("META : "+ metaType + " -- "+ metaName + " -- " +  getMetaContentProperty() + " -- " + metaHint);
    }
    // ---------------------------------------------------------------------------------
    public String getMetaName() {
        return metaName;
    }

    public String getMetaHint() {
        return metaHint;
    }

    public boolean isMetaType() {
        return metaType;
    }

    public StringProperty carContentProperty() {
        return metaContentProperty;
    }

    public final String getMetaContentProperty() {
        return carContentProperty().get() ;
    }

    public final void setMetaContentProperty(String carContent) {
        carContentProperty().set(carContent);
    }
}
