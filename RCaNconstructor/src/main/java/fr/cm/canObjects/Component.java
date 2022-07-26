/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import fr.cm.GUInetwork.ComponentGroup;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.Strings;


public class Component extends ComponentGroup {
    // ---------------------------------
    private final int np;
    private final double[] parameters;
    // --------------------------------------------
    public Component() {
        super();
        np = Strings.getNumberOfParameters();
        parameters = new double[np];
    }
    // --------------------------------------------
    public Component(String name, double[] param, boolean type, double x, double y) {
        super( name, type,  x,  y);
        np = Strings.getNumberOfParameters();
        parameters = new double[np];
        System.arraycopy(param,0,parameters,0,np);
    }
    // --------------------------------------------
    public void xsetParameters(int p, String newV) {
        try {
            double val = Double.parseDouble(newV);
            parameters[p] = val;
        } catch (NumberFormatException e) {
            parameters[p] = 0.0;
        }
    }
    public double[] getParameters() {
        return (parameters);
    }

    public double getParameters(int p) {
        return (parameters[p]);
    }

    public void setParameters(double[] param) {
        System.arraycopy(param,0,parameters,0,np);
    }

    // --------------------------------------------

    public void changeParameters(int p, String newVal){
        double nVal;
        try {
            nVal = Double.parseDouble(newVal);
            if ((Math.abs(nVal - parameters[p]) > 0.0000001) && nVal>=0.0) {
                ProjectListsManager.addAction("Change value of parameter "
                        + Strings.getParametersNames(p)+ " for component  "
                        + this.getName() + " : " + parameters[p] + " -> " + newVal, true);
                parameters[p] = nVal;
            }
        }
        catch (NumberFormatException e) {

        }
    }
}
