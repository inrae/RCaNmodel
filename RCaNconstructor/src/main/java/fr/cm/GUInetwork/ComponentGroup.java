package fr.cm.GUInetwork;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.event.EventHandler;
import javafx.scene.Cursor;
import javafx.scene.Group;
import javafx.scene.input.MouseEvent;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.text.TextAlignment;

public class ComponentGroup extends Group {
        // Group : javafx class pour les objets graphiques
        private String name;
        private boolean inside;
        private Circle circle;
        private Text text;
        private double x,y;

        Color col;

        // --------------------------------------------
        public ComponentGroup() {
            super();
            this.name = "";
            inside = true;
            this.circle = new Circle();
            this.text = setText();
            this.x = 0.5;
            this.y = 0.5;
            setPos();
        }
        // --------------------------------------------
        public ComponentGroup(String name,  boolean type, double x, double y) {
            super();
            this.setName(name);
            this.inside = type;
            this.circle = new Circle();
            this.text = setText();
            this.x = x;
            this.y = y;
            setPos();
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


        public void refill() {
            circle.setFill(col);
        }

        public String getName() { return (name); }

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
                ProjectListsManager.addAction("Change component name : " + name + " -> "+ newName, true);
                name = newName;
            }
        }
        public void changeInside(boolean newInside){
            if(!inside==newInside){
                ProjectListsManager.addAction(
                        "Change position of component  " + name + " : inside "+ inside +" -> "+ newInside,true);
                inside = newInside;
            }
        }



}
