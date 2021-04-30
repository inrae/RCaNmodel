package fr.cm.menus;

import fr.cm.dialogs.MetaInformationSave;
import fr.cm.dialogs.*;
import fr.cm.tables.*;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuMeta {

    static final MenuItem projectItem = new MenuItem("Model description");
    static final MenuItem saveMetaTxt = new MenuItem("Save meta information");
    static final MenuItem fileWithObservationItem = new MenuItem("About ibservation files");

    static List<MenuItem> menuItems = null;

    static BorderPane borderPaneRacine;

    public MenuMeta(BorderPane borderPaneRacine) {
        this.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(projectItem, fileWithObservationItem, saveMetaTxt);
        for (MenuItem menuItem : menuItems) {
            menuItem.setOnAction(MetaListener);
        }
    }

    public static void updateMenus() {
        boolean notStarted = !Context.isStarted();
        for (MenuItem menuItem : menuItems) {
            menuItem.setDisable(notStarted);
        }
    }

    static void disableMetaMenus(boolean disable) {
        for (MenuItem menuItem : menuItems) {
            menuItem.setDisable(disable);
        }
    }

    public static List<MenuItem> getMenuItems() {
        return menuItems;
    }

    static final EventHandler<ActionEvent> MetaListener = e -> handle(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 :
                MetaInformationPane dialogForProject = new MetaInformationPane();
                borderPaneRacine.setCenter(dialogForProject);
                break;
            case 1:
                ExternalDataFileDialog externalDataFileDialog = new ExternalDataFileDialog();
                borderPaneRacine.setCenter(externalDataFileDialog);
                break;
            case 2:
                new MetaInformationSave();
                break;
            default:
        }
    }
}


