package fr.cm.GUInetwork;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.Component;
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
import javafx.scene.shape.*;

public class FluxGroup extends Group {
    // Group : javafx class pour les objets graphiques
    Component in;
    Component out;
    Color col;
    Path arrow;
    Path path;
    Circle circle;
    boolean typeTrophic;

    // --------------------------------------------
    public FluxGroup(String i, String o, boolean t) {
        in = ProjectListsManager.whichComponent(i);
        out = ProjectListsManager.whichComponent(o);
        typeTrophic = t;
    }

    // --------------------------------------------
    public FluxGroup(Component i, Component o, boolean t) {
        this.in = i;
        this.out = o;
        typeTrophic = t;
    }

    // ---------------------------------------------------------------------
    public void define(EventHandler<MouseEvent> mouseDoubleClickeOnLineEventHandler) {
        if (in == out) {
            circle = new Circle();
            circle.centerXProperty().bind(in.getCircle().centerXProperty());
            circle.centerYProperty().bind(in.getCircle().centerYProperty());
            circle.setRadius(Context.getRadiusInternalLink());
            getChildren().add(circle);
        } else {
            path = new Path();
            MoveTo start = new MoveTo();
            start.xProperty().bind(in.getCircle().centerXProperty());
            start.yProperty().bind(in.getCircle().centerYProperty());
            LineTo end = new LineTo();
            end.xProperty().bind(out.getCircle().centerXProperty());
            end.yProperty().bind(out.getCircle().centerYProperty());
            path.getElements().add(start);
            path.getElements().add(end);
            arrow = getArrow();
            getChildren().add(path);
            getChildren().add(arrow);
            setOnMouseClicked(mouseDoubleClickeOnLineEventHandler);
        }
        setCursor(Cursor.CROSSHAIR);
        fillColor();
    }

    // --------------------------------------------
    public boolean isTypeTrophic() {
        return typeTrophic;
    }

    // --------------------------------------------
    public void setTypeTrophic(boolean type) {
        typeTrophic = type;
        fillColor();
    }

    // --------------------------------------------
    private void fillColor() {
        if (typeTrophic) {
            col = ColorsAndFormats.linkTrophic;
        } else {
            col = ColorsAndFormats.linkNonTrophic;
        }
        if (in == out) {
            circle.setStroke(col);
            circle.setStrokeWidth(10.0);
            circle.setFill(col);
        } else {
            path.setStroke(col);
            path.setStrokeWidth(10.0);
            arrow.setStroke(col);
            arrow.setStrokeWidth(1.0);
            arrow.setFill(col);
        }
    }

    // --------------------------------------------
    public String getName() {
        return (in.getName()+"_"+out.getName());
    }

    public String getInName() {
        return (in.getName());
    }

    public String getOutName() {
        return (out.getName());
    }

    public Component getIn() {
        return (in);
    }

    public Component getOut() {
        return (out);
    }

    public boolean hasExtremities(Component component) {
        return (component.equals(out) || component.equals(in));
    }

    public static final IntegerProperty ARROW_LENGTH = new SimpleIntegerProperty(20);

    // --------------------------------------------
    private Path getArrow() {
        ObservableList<PathElement> list = this.path.getElements();
        // extremite
        DoubleBinding tipX = ((LineTo) list.get(1)).xProperty().add(0);
        DoubleBinding tipY = ((LineTo) list.get(1)).yProperty().add(0);
        // origine
        DoubleBinding buttX = ((MoveTo) list.get(0)).xProperty().add(0);
        DoubleBinding buttY = ((MoveTo) list.get(0)).yProperty().add(0);

        DoubleProperty mX = new SimpleDoubleProperty();
        DoubleProperty mY = new SimpleDoubleProperty();
        mX.bind(tipX.add(buttX).multiply(0.5));
        mY.bind(tipY.add(buttY).multiply(0.5));

        DoubleProperty dx = new SimpleDoubleProperty();
        DoubleProperty dY = new SimpleDoubleProperty();
        dx.bind(tipY.subtract(buttY));
        dY.bind(tipX.subtract(buttX));

        MoveTo left = new MoveTo();
        left.xProperty().bind(mX.add(new fr.cm.GUInetwork.FluxGroup.CosBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI + Math.PI / 4)).multiply(ARROW_LENGTH)));
        left.yProperty().bind(mY.add(new fr.cm.GUInetwork.FluxGroup.SinBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI + Math.PI / 4)).multiply(ARROW_LENGTH)));

        LineTo tip = new LineTo();
        tip.xProperty().bind(mX);
        tip.yProperty().bind(mY);

        LineTo right = new LineTo();
        right.xProperty().bind(mX.add(new fr.cm.GUInetwork.FluxGroup.CosBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI - Math.PI / 4)).multiply(ARROW_LENGTH)));
        right.yProperty().bind(mY.add(new fr.cm.GUInetwork.FluxGroup.SinBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI - Math.PI / 4)).multiply(ARROW_LENGTH)));

        LineTo re = new LineTo();
        re.xProperty().bind(mX.add(new fr.cm.GUInetwork.FluxGroup.CosBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI + Math.PI / 4)).multiply(ARROW_LENGTH)));
        re.yProperty().bind(mY.add(new fr.cm.GUInetwork.FluxGroup.SinBinding(new fr.cm.GUInetwork.FluxGroup.ArcTanBinding(dx, dY).add(Math.PI + Math.PI / 4)).multiply(ARROW_LENGTH)));

        Path temp = new Path();
        temp.getElements().add(left);
        temp.getElements().add(tip);
        temp.getElements().add(right);
        temp.getElements().add(re);

        temp.getStyleClass().setAll("edge");
        return temp;
    }

    // --------------------------------------------
    private static class ArcTanBinding extends DoubleBinding {
        private final DoubleProperty x, y;
        public ArcTanBinding(DoubleProperty x, DoubleProperty y) {
            this.x = x;
            this.y = y;
            super.bind(this.x);
            super.bind(this.y);
        }
        @Override
        protected double computeValue() {

            return Math.atan2(this.x.get(), this.y.get());
        }
    }

    // --------------------------------------------
    private static class SinBinding extends DoubleBinding {
        private final DoubleBinding theta;
        public SinBinding(DoubleBinding theta) {
            this.theta = theta;
            super.bind(this.theta);
        }

        @Override
        protected double computeValue() {
            return Math.sin(this.theta.get());
        }
    }
    // --------------------------------------------
    private static class CosBinding extends DoubleBinding {
        private final DoubleBinding theta;
        public CosBinding(DoubleBinding theta) {
            this.theta = theta;
            super.bind(this.theta);
        }
        @Override
        protected double computeValue() {
            return Math.cos(this.theta.get());
        }
    }
    // --------------------------------------------
    public void changeTypeTrophic(boolean newTypeTrophic){
        if(!typeTrophic==newTypeTrophic){
            ProjectListsManager.addAction(
                    "Change type of flux " + getName() + " : trophic "+ typeTrophic +" -> "+ newTypeTrophic, false);
            typeTrophic = newTypeTrophic;
        }
    }

}
