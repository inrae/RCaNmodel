package fr.cm.Main;

import fr.cm.objects.ComponentTable;
import fr.cm.objects.ConstraintTable;
import fr.cm.objects.DataFileTable;
import fr.cm.objects.FluxTable;
import fr.cm.objects.ObservationTable;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuTrophicNetwork {
// ---------------------------------

    static final MenuItem networkItem = new MenuItem("Network"),
            groupsItem = new MenuItem("Components"),
            linksItem = new MenuItem("Fluxes"),
            constraintsItem = new MenuItem("Constraints"),
            observationsItem = new MenuItem("Observations"),
            dataFileItem = new MenuItem("Data Files");
    static List<MenuItem> menuItems = null;
    static BorderPane borderPaneRacine;
    public MenuTrophicNetwork(BorderPane borderPaneRacine) {
        MenuTrophicNetwork.borderPaneRacine = borderPaneRacine;
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
    static final EventHandler<ActionEvent> ViewListener = MenuTrophicNetwork::handle;
    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 -> {
                ObjectsManager.getNetworkView().update();
                borderPaneRacine.setCenter(ObjectsManager.getNetworkView());
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

