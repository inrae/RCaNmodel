/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import fr.cm.parameters.Strings;
import javafx.event.EventHandler;
import javafx.scene.Group;
import javafx.scene.Cursor;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;

/**
 * @author christianmullon
 */

public class Component extends Group {
    // Group : javafx class pour les objets graphiques
    private String name;
    private boolean inside;
    private final int np;
    private final double[] parameters;
    private final Circle circle;
    private Text text;
    private double x,y;

    Color col;

    // --------------------------------------------
    public Component() {
        super();
        this.name = "";
        inside = true;
        this.circle = new Circle();
        this.text = setText();
        np = Strings.getNumberOfParameters();
        parameters = new double[np];
        this.x = 0.5;
        this.y = 0.5;
        setPos();
    }
    // --------------------------------------------
    public Component(String name, double[] param, boolean type, double x, double y) {
        super();
        this.setName(name);
        this.inside = type;
        this.circle = new Circle();
        this.text = setText();
        this.x = x;
        this.y = y;
        setPos();
        np = Strings.getNumberOfParameters();
        parameters = new double[np];
        System.arraycopy(param,0,parameters,0,np);
    }
    // --------------------------------------------
    public Text setText() {
        text = new Text();
        text.xProperty().bind(circle.centerXProperty());
        text.yProperty().bind(circle.centerYProperty());
        text.setText(name);
        text.setTextAlignment(TextAlignment.CENTER);
        text.setFont(Font.font("Verdana", 20));
        text.setFill(ColorsAndFormats.names);
        return text;
    }
    // --------------------------------------------
    boolean insideRect(double mi, double ma){
        return(x >= mi && x <= ma && y >= mi && y <= ma);
    }
    // --------------------------------------------
    void reposInsideOutside() {
        double alpha;
        double mi = Context.getInsideMin();
        double ma = Context.getInsideMax();
        boolean posInside = insideRect(mi, ma);
        if (inside) {
            if ( ! posInside)  {
                alpha = 100000;
                if (x >= ma) {
                    alpha = Math.min(alpha, (x - ma) / (x - 0.5));
                }
                if (x <= mi) {
                    alpha = Math.min(alpha, (x - mi) / (x - 0.5));
                }
                if (y >= ma) {
                    alpha = Math.min(alpha, (y - ma) / (y - 0.5));
                }
                if (y <= mi) {
                    alpha = Math.min(alpha, (y - mi) / (y - 0.5));
                }
                x = (x - alpha * (x - 0.5)) ;
                y = (y - alpha * (y - 0.5));
            }
        } else {
            if ( posInside)  {
                alpha = -100000;
                if (x > 0.5) {
                    alpha = Math.max(alpha, (x - ma) / (x - 0.5));
                }
                if (x < 0.5) {
                    alpha = Math.max(alpha, (x - mi) / (x - 0.5));
                }
                if (y > 0.5) {
                    alpha = Math.max(alpha, (y - ma) / (y - 0.5));
                }
                if (y < 0.5) {
                    alpha = Math.max(alpha, (y - mi) / (y - 0.5));
                }
                x = (x - alpha * (x - 0.5)) ;
                y = (y - alpha * (y - 0.5)) ;
            }
        }
    }
    // --------------------------------------------
    public void setPos() {
        reposInsideOutside();
        circle.setCenterX(x * Context.getBackgroundWidth());
        circle.setCenterY(y * Context.getBackgroundHeight());
    }
    // --------------------------------------------
    public void setWH(double wx, double hy){
        x = wx / Context.getBackgroundWidth();
        y = hy / Context.getBackgroundHeight();
        setPos();
    }
// --------------------------------------------
    public void setXY(double x, double y) {
        this.x = x;
        this.y = y;
        setPos();
    }
    // --------------------------------------------
    public void setInside(boolean inside) {
        this.inside = inside;
        reposInsideOutside();
        setCol();
        setPos();
    }
    // --------------------------------------------
    public void define(
            EventHandler<MouseEvent> mousePressedEventHandler,
            EventHandler<MouseEvent> mouseDraggedEventHandler,
            EventHandler<MouseEvent> mouseDoubleClickeOnCircleEventHandler
    ) {
        circle.setRadius(Context.getRadiusComponent());
        this.setCol();
        circle.setCursor(Cursor.CROSSHAIR);
        circle.setOnMousePressed(mousePressedEventHandler);
        circle.setOnMouseDragged(mouseDraggedEventHandler);
        circle.setOnMouseClicked(mouseDoubleClickeOnCircleEventHandler);
        getChildren().add(circle);
        getChildren().add(text);
    }
    // --------------------------------------------
    public void setCol() {
        if (inside) {
            col = ColorsAndFormats.componentInside;
        } else {
            col = ColorsAndFormats.componentOutside;
        }
        circle.setFill(col);
    }
    // --------------------------------------------
    public Circle getCircle() {
        return (circle);
    }

    public boolean isInside() {
        return inside;
    }

    public void setParameters(int p, String newV) {
        try {
            double val = Double.parseDouble(newV);
            parameters[p] = val;
        } catch (NumberFormatException e) {
            parameters[p] = 0.0;
        }
    }

    public void refill() {
        circle.setFill(col);
    }

    public String getName() { return (name); }

    public double[] getParameters() { return (parameters); }

    public double getParameters(int p) { return (parameters[p]); }

    public void setParameters(double[] param) { System.arraycopy(param,0,parameters,0,np); }

    public void setName(String name) {
        this.name = name.replace(" ","").replace("-","");
    }

    public double getX() {
        return x;
    }

    public double getY() {
        return y;
    }
    // --------------------------------------------
    public void changeName(String newName){
        if(!name.equals(newName)){
            ProjectListsManager.addAction("Change component name : " + name + " -> "+ newName);
            name = newName;
        }
    }
    public void changeInside(boolean newInside){
        if(!inside==newInside){
            ProjectListsManager.addAction("Change position of component  " + name + " : inside "+ inside +" -> "+ newInside);
            inside = newInside;
        }
    }

    public void changeParameters(int p, String newVal){
        double nVal;
        try {
            nVal = Double.parseDouble(newVal);
            if ((Math.abs(nVal - parameters[p]) > 0.0000001) && nVal>=0.0) {
                ProjectListsManager.addAction("Change value of parameter "
                        + Strings.getParametersNames(p)+ " for component  "
                        + name + " : " + parameters[p] + " -> " + newVal);
                parameters[p] = nVal;
            }
        }
        catch (NumberFormatException e) {

        }
    }
}
