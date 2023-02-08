package fr.cm.Main;

import fr.cm.objects.TimeLineTable;
import fr.cm.objects.ComponentTable;
import fr.cm.objects.ConstraintTable;
import fr.cm.objects.DataFileTable;
import fr.cm.objects.FluxTable;
import fr.cm.objects.MetaInformationTable;
import fr.cm.objects.ObservationTable;
import fr.cm.project.ProjectListsManager;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuSetUp {
// ---------------------------------

    static final MenuItem networkItem = new MenuItem("Network");
    static final MenuItem groupsItem = new MenuItem("Components");
    static final MenuItem linksItem = new MenuItem("Fluxes");
    static final MenuItem constraintsItem = new MenuItem("Constraints");
    static final MenuItem observationsItem = new MenuItem("Observations");
    static final MenuItem dataFileItem = new MenuItem("Data Files");
    static List<MenuItem> menuItems = null;

    static BorderPane borderPaneRacine;

    public MenuSetUp(BorderPane borderPaneRacine) {
        MenuSetUp.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(networkItem, groupsItem, linksItem, dataFileItem, observationsItem, constraintsItem);
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

    static final EventHandler<ActionEvent> ViewListener = MenuSetUp::handle;

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
            default -> {
            }
        }
    }
}

