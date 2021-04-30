package fr.cm.menus;

import fr.cm.canObjects.Project;
import fr.cm.tables.ComponentTable;
import fr.cm.tables.ConstraintTable;
import fr.cm.tables.LinkTable;
import fr.cm.tables.ObservationTable;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuView {

    static final MenuItem networkItem = new MenuItem("Network");
    static final MenuItem groupsItem = new MenuItem("Components");
    static final MenuItem linksItem = new MenuItem("Fluxes");
    static final MenuItem constraintsItem = new MenuItem("Constraints");
    static final MenuItem observationsItem = new MenuItem("Observations");

    static List<MenuItem> menuItems = null;

    static BorderPane borderPaneRacine;

    public MenuView(BorderPane borderPaneRacine) {
        this.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(networkItem, groupsItem, linksItem, observationsItem, constraintsItem
        );
        for (MenuItem menuItem : menuItems) {
            menuItem.setOnAction(ViewListener);
        }
    }

    static void updateMenus() {
        boolean notStarted = !Context.isStarted();
        for (MenuItem menuItem : menuItems) {
            menuItem.setDisable(notStarted);
        }
     }

    public static List<MenuItem> getMenuItems() {
        return menuItems;
    }

    static final EventHandler<ActionEvent> ViewListener = e -> handle(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 :
                Project.getNetworkView().update();
                borderPaneRacine.setCenter(Project.getNetworkView());
                break;
            case 1:
                ComponentTable componentTable = new ComponentTable();
                borderPaneRacine.setCenter(componentTable);
                break;
            case 2 :
                LinkTable linkTable = new LinkTable();
                borderPaneRacine.setCenter(linkTable);
                break;
            case 3:
                ObservationTable observationTable = new ObservationTable();
                borderPaneRacine.setCenter(observationTable);
                break;
            case 4 :
                ConstraintTable constraintTable = new ConstraintTable();
                borderPaneRacine.setCenter(constraintTable);
                break;
            default:
        }
    }
}

