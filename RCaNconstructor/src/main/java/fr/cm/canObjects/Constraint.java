/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import fr.cm.ProjectManager.ProjectListsManager;
import javafx.beans.property.SimpleStringProperty;

public class Constraint {

    final SimpleStringProperty formula = new SimpleStringProperty();
    final SimpleStringProperty name = new SimpleStringProperty();
    final SimpleStringProperty years = new SimpleStringProperty();
    final SimpleStringProperty comment = new SimpleStringProperty();
    boolean active;

    public Constraint(String name, String formula, String years,
                      boolean act, String comment) {
        this.years.set(years);
        this.name.set(name);
        this.formula.set(formula.replace(" ",""));
        this.comment.set(comment);
        active = act;
    }

    public String getName(){ return(name.get()); }

    public String getFormula() { return (formula.get()); }

    public boolean involve(Flux flux) { return (getFormula().contains(flux.getName())); }

    public boolean involve(Component component) { return (getFormula().contains(component.getName())); }

    public String getYears() { return years.get(); }

    public boolean isActive() { return active; }

    public void setActive(boolean act) { active= act; }

    public void setComment(String comment) { this.comment.set(comment); }

    public String getComment() { return (comment.get()); }

    public void setName(String name) { this.name.set(name);}

    public void changeName(String newName){
        if(! this.getName().equals(newName)){
            ProjectListsManager.addAction("Change constraint name : " + name + " -> "+ newName, true);
            setName(newName);
        }
    }
    public void changeActive(boolean newActive){
        if(!active==newActive){
            ProjectListsManager.addAction("Change position of component  " + name + " : active "+ active +" -> "+ newActive, true);
            active = newActive;
        }
    }

}
