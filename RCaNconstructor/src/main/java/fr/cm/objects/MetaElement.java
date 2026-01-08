/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.objects;

import fr.cm.Main.ObjectsManager;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

/**
 * @author christianmullon
 */
public class MetaElement {
    final String metaName;
     final StringProperty metaContent = new SimpleStringProperty(),
             metaHint = new SimpleStringProperty();
    final BooleanProperty metaType = new SimpleBooleanProperty();
    // --------------------------------------------
    public MetaElement(String elementR) {
        if(elementR.startsWith("<")){
            metaType.set(false);
            this.metaName = elementR;
            setMetaContent("");
            setMetaHint("");
        }
        else {
            metaType.set(true);
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
    public String getMetaName() {return metaName;}
    public boolean isMetaType() {return metaType.get();}
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
