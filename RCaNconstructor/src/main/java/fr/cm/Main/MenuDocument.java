package fr.cm.Main;

import fr.cm.objects.TimeLineTable;
import fr.cm.objects.MetaInformationTable;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuDocument {
// ---------------------------------
   static final MenuItem metaTableItem = new MenuItem("Project information");
    static final MenuItem timeLinesItem = new MenuItem("Project tracking");
    static List<MenuItem> menuItems = null;
    static BorderPane borderPaneRacine;
    public MenuDocument(BorderPane borderPaneRacine) {
        MenuDocument.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList( timeLinesItem, metaTableItem);
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
    static final EventHandler<ActionEvent> ViewListener = MenuDocument::handle;
    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 -> {
                TimeLineTable timeLineTable = new TimeLineTable();
                borderPaneRacine.setCenter(timeLineTable);
            }
            case 1 -> {
                MetaInformationTable metaInformationTable = new MetaInformationTable();
                borderPaneRacine.setCenter(metaInformationTable);
            }
            default -> {
            }
        }
    }
}

