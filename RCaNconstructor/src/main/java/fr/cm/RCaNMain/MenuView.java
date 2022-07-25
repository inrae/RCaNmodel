package fr.cm.RCaNMain;

import fr.cm.GUItablesViews.*;
import fr.cm.ProjectManager.ProjectListsManager;
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
    static final MenuItem dataFileItem = new MenuItem("Data Files");
    static final MenuItem metaTableItem = new MenuItem("Project information");
    static final MenuItem actionsItem = new MenuItem("Project tracking");

    static List<MenuItem> menuItems = null;

    static BorderPane borderPaneRacine;

    public MenuView(BorderPane borderPaneRacine) {
        MenuView.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(networkItem, groupsItem, linksItem, dataFileItem, observationsItem, constraintsItem, actionsItem, metaTableItem);
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

    static final EventHandler<ActionEvent> ViewListener = MenuView::handle;

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 -> {
                ProjectListsManager.getNetworkView().update();
                borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
            }
            case 1 -> {
                ComponentTable componentTable = new ComponentTable();
                borderPaneRacine.setCenter(componentTable);
            }
            case 2 -> {
                FluxTable fluxTable = new FluxTable();
                borderPaneRacine.setCenter(fluxTable);
            }
            case 3 -> {
                DataFileTable dataFileTable = new DataFileTable();
                borderPaneRacine.setCenter(dataFileTable);
            }
            case 4 -> {
                ObservationTable observationTable = new ObservationTable();
                borderPaneRacine.setCenter(observationTable);
            }
            case 5 -> {
                ConstraintTable constraintTable = new ConstraintTable();
                borderPaneRacine.setCenter(constraintTable);
            }
               case 6 -> {
                ActionTable actionTable = new ActionTable();
                borderPaneRacine.setCenter(actionTable);
            }
            case 7 -> {
                MetaInformationTable metaInformationTable = new MetaInformationTable();
                borderPaneRacine.setCenter(metaInformationTable);
            }
            default -> {
            }
        }
    }
}

