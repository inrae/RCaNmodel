/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import fr.cm.GUInetwork.FluxGroup;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.binding.DoubleBinding;
import javafx.beans.property.DoubleProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleIntegerProperty;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.scene.Cursor;
import javafx.scene.Group;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.LineTo;
import javafx.scene.shape.MoveTo;
import javafx.scene.shape.Path;
import javafx.scene.shape.PathElement;

/**
 *
 * @author christianmullon
 */
public class Flux extends FluxGroup {
    // --------------------------------------------
    public Flux(String i, String o, boolean t) {

        super(i,o,t);
    }
    // --------------------------------------------
    public Flux(Component i, Component o, boolean t) {

        super(i,o,t);
    }
    // ---------------------------------------------------------------------

}
