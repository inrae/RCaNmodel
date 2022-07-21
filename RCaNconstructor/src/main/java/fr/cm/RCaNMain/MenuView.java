package fr.cm.RCaNMain;

import fr.cm.GUItablesViews.*;
import fr.cm.canObjects.ProjectListsManager;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuView {

    static final MenuItem networkItem = new MenuItem("Network");
    static final MenuItem actionsItem = new MenuItem("Actions");
    static final MenuItem groupsItem = new MenuItem("Components");
    static final MenuItem linksItem = new MenuItem("Fluxes");
    static final MenuItem constraintsItem = new MenuItem("Constraints");
    static final MenuItem observationsItem = new MenuItem("Observations");
    static final MenuItem dataFileItem = new MenuItem("Data Files");

    static List<MenuItem> menuItems = null;

    static BorderPane borderPaneRacine;

    public MenuView(BorderPane borderPaneRacine) {
        this.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(networkItem, groupsItem, linksItem, observationsItem, constraintsItem, dataFileItem, actionsItem
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
                ProjectListsManager.getNetworkView().update();
                borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
                break;
            case 1:
                ComponentTable componentTable = new ComponentTable();
                borderPaneRacine.setCenter(componentTable);
                break;
            case 2 :
                FluxTable fluxTable = new FluxTable();
                borderPaneRacine.setCenter(fluxTable);
                break;
            case 3:
                ObservationTable observationTable = new ObservationTable();
                borderPaneRacine.setCenter(observationTable);
                break;
            case 4 :
                ConstraintTable constraintTable = new ConstraintTable();
                borderPaneRacine.setCenter(constraintTable);
                break;
            case 5 :
                DataFileTable dataFileTable = new DataFileTable();
                borderPaneRacine.setCenter(dataFileTable);
                break;
            case 6 :
                ActionTable actionTable = new ActionTable();
                borderPaneRacine.setCenter(actionTable);
                break;
            default:
        }
    }
}

