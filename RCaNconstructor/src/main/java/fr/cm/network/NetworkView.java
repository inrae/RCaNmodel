package fr.cm.network;

import fr.cm.canObjects.Component;
import fr.cm.canObjects.Flux;
import fr.cm.canObjects.ListsManager;
import fr.cm.menus.Context;
import fr.cm.menus.MainApplication;

import fr.cm.parameters.Colors;

import java.util.List;

import javafx.collections.ObservableList;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.shape.Circle;
import javafx.scene.shape.Rectangle;

/**
 * @author christianmullon
 */
public class NetworkView extends Pane {

    boolean addingTrophicLink = false;
    boolean addingNonTrophicLink = false;
    Component originOfLink = null;
    Component clickedComponent = null;
    double h, w;

    Rectangle fond, interieur;
    double positionBeforeX, positionBeforeY;

    public NetworkView(){
        super();
    }
    // ---------------------------------------------------------------------
    public void init() {
        fond = background();
        interieur = inside();
        getChildren().addAll(fond, interieur);
        resetLink();
    }

    // ---------------------------------------------------------------------
    private Rectangle background() {
        w = Context.getWidth();
        h = 0.9 * Context.getHeight();
        Rectangle rect = new Rectangle();
        rect.setX(0);
        rect.setY(0);
        rect.setWidth(w);
        rect.setHeight(h);
        Color col = Colors.background;
        rect.setFill(col);
        rect.setStroke(Colors.background);
        rect.setStrokeWidth(10);
        rect.setOnMouseClicked(mouseDoubleClickOnBcgEventHandler);
        return rect;
    }

    private Rectangle inside() {
        w = Context.getWidth();
        h = 0.9 * Context.getHeight();
        Rectangle rect = new Rectangle();

        double mi = Context.getInsideMin();
        double ma = Context.getInsideMax();
        rect.setX(mi * w);
        rect.setY(mi * h);
        rect.setWidth((ma-mi) * w);
        rect.setHeight((ma-mi) * h);

        Color col = Colors.inside;
        rect.setFill(col);
        rect.setStroke(Colors.inside);
        rect.setStrokeWidth(1);
        rect.setOnMouseClicked(mouseDoubleClickOnBcgEventHandler);
        return rect;
    }

    public void update() {
        ObservableList<Node> components = this.getChildren();
        this.getChildren().removeAll(components);
        List<Component> listOfComponents = ListsManager.getListOfComponents();
        for(Component component: listOfComponents){
            component.setPos();
        }
        List<Flux> listOfFluxes = ListsManager.getListOfFluxes();
        fond = background();
        interieur = inside();
        this.getChildren().addAll(fond, interieur);
        this.getChildren().addAll(listOfFluxes);
        this.getChildren().addAll(listOfComponents);
        if(clickedComponent != null){
            clickedComponent.setCol();
        }
        resetLink();
     }

    void resetLink() {
        addingTrophicLink = false;
        addingNonTrophicLink = false;
        originOfLink = null;
    }

    public void redrawChangingSize() {
        double nw = MainApplication.stage.getWidth();
        double nh = MainApplication.stage.getHeight();
        Context.setWidth(nw);
        Context.setHeight(nh);
        if (nw > 1.0 && nh > 1.0) {
            update();
        }
    }

    // -------------------------------------------------------------------------------------
    private final EventHandler<MouseEvent> mousePressedEventHandler = (t)
            -> {
        positionBeforeX = t.getSceneX();
        positionBeforeY = t.getSceneY();
    };
    // -------------------------------------------------------------------------------------
    private final EventHandler<MouseEvent> mouseDraggedEventHandler = (t)
            -> {
        double deplacementX = t.getSceneX() - positionBeforeX;
        double deplacementY = t.getSceneY() - positionBeforeY;
        Circle circle = (Circle) (t.getSource());
        Component component = (Component) (circle.getParent());
        component.refill();
        double nx = circle.getCenterX() + deplacementX;
        double ny = circle.getCenterY() + deplacementY;
        component.setWH(nx,ny);
        positionBeforeX = t.getSceneX();
        positionBeforeY = t.getSceneY();
    };

    // -------------------------------------------------------------------------------------
    final EventHandler<MouseEvent> mouseDoubleClickOnBcgEventHandler = (t)
            -> {
        if (t.getClickCount() == 2) {
            new ComponentNew(t.getX(), t.getY());
            this.update();
            resetLink();
        }
    };
    private final EventHandler<MouseEvent> mouseDoubleClickOnLineEventHandler = (var t)
            -> {
        if (t.getClickCount() == 2) {
            Flux flux = (Flux) (t.getSource());
            new LinkEdit(flux);
            this.update();
        }
    };
    private final EventHandler<MouseEvent> mouseDoubleClickOnCircleEventHandler = (var t)
            -> {
        positionBeforeX = t.getSceneX();
        positionBeforeY = t.getSceneY();
        Circle circle = (Circle) (t.getSource());
        clickedComponent = (Component) (circle.getParent());
        if (originOfLink == null) {
            if(t.getClickCount()>1) {
                new ComponentClic(clickedComponent, this);
             }
        } else {
            if (addingTrophicLink) {
                Flux flux = new Flux(originOfLink, clickedComponent, true);
                ListsManager.addLink(flux);
                resetLink();
                update();
            }
            if (addingNonTrophicLink) {
                Flux flux = new Flux(originOfLink, clickedComponent, false);
                ListsManager.addLink(flux);
                resetLink();
                update();
            }
        }
    };

    public EventHandler<MouseEvent> getMouseDoubleClickOnCircleEventHandler() {
        return mouseDoubleClickOnCircleEventHandler;
    }

    public EventHandler<MouseEvent> getMouseDoubleClickOnLineEventHandler() {
        return mouseDoubleClickOnLineEventHandler;
    }

    public EventHandler<MouseEvent> getMousePressedEventHandler() {
        return mousePressedEventHandler;
    }

    public EventHandler<MouseEvent> getMouseDraggedEventHandler() {
        return mouseDraggedEventHandler;
    }

    public  void setAddingTrophicLink(boolean addingTrophicLink) {
        originOfLink = clickedComponent;
        this.addingTrophicLink = addingTrophicLink;
    }

    public void setAddingNonTrophicLink(boolean addingNonTrophicLink) {
        originOfLink = clickedComponent;
        this.addingNonTrophicLink = addingNonTrophicLink;
    }

    public  void setOriginOfLink(Component origin) {
        this.originOfLink = origin;
    }


}
