package fr.cm.RCaNMain;

import fr.cm.canObjects.ProjectListsManager;
import fr.cm.GUIdialogs.*;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuFile {

    static final MenuItem newItem = new MenuItem("New");
    static final MenuItem openFileItem = new MenuItem("Open");
    static final MenuItem saveFileItem = new MenuItem("Save");
    static final MenuItem saveAsFileItem = new MenuItem("Save as ...");
    static final MenuItem exitItem = new MenuItem("Exit");
    static BorderPane borderPaneRacine;

    static List<MenuItem> menuItems = null;

    public MenuFile(BorderPane borderPaneRacine) {
        this.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(newItem, openFileItem, saveFileItem, saveAsFileItem, exitItem);
        for (MenuItem menuItem : menuItems) {
            menuItem.setOnAction(FileListener);
        }
    }

    public static void updateMenus() {
        boolean notStarted = !Context.isStarted();
        newItem.setDisable(false);
        openFileItem.setDisable(false);
        saveFileItem.setDisable(notStarted);
        saveAsFileItem.setDisable(notStarted);
        exitItem.setDisable(false);
    }

    public static List<MenuItem> getMenuItems() {
        return menuItems;
    }

    static final EventHandler<ActionEvent> FileListener = e -> handle(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 :  // new project
                // System.out.println("new project");
                new ProjectCreateNew();
                if (Context.isStarted()) {
                    Context.initRCaN();
                    ProjectListsManager.init();
                    MainApplication.updateMenus();
                    borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
                }
                break;
            case 1: // open project
                // System.out.println("open project");
                new ProjectOpenExisting();
                if (Context.isStarted()) {
                    Context.initRCaN();
                    ProjectListsManager.init();
                    MainApplication.updateMenus();
                    ProjectListsManager.getExcel();
                    borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
                }
                break;
            case 2: // save project
                // System.out.println("save project");
                if (Context.isStarted()) {
                    if(Context.getDirName().length() == 0){
                        new ProjectChangeFileName();
                    }
                    ProjectListsManager.saveExcel();
                    Context.setChanged(false);
                }
                break;
            case 3: // save as...
                if (Context.isStarted()) {
                    new ProjectChangeFileName();
                    ProjectListsManager.saveExcel();
                    Context.setChanged(false);
                }
                break;
            case 4 : // exit
                MainApplication.close();
                break;
            default:
        }
    }
}


