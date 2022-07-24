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
    static final MenuItem closeFileItem = new MenuItem("Close");
    static final MenuItem exitItem = new MenuItem("Exit");
    static BorderPane borderPaneRacine;

    static List<MenuItem> menuItems = null;

    public MenuFile(BorderPane borderPaneRacine) {
        MenuFile.borderPaneRacine = borderPaneRacine;
        menuItems = Arrays.asList(newItem, openFileItem, closeFileItem, saveFileItem, saveAsFileItem, exitItem);
        for (MenuItem menuItem : menuItems) {
            menuItem.setOnAction(FileListener);
        }
    }

    public static void updateMenus() {
        boolean started = Context.isStarted();
        newItem.setDisable(started);
        openFileItem.setDisable(started);
        closeFileItem.setDisable(! started);
        saveFileItem.setDisable(! started);
        saveAsFileItem.setDisable(! started);
        exitItem.setDisable(false);
    }

    public static List<MenuItem> getMenuItems() {
        return menuItems;
    }

    static final EventHandler<ActionEvent> FileListener = MenuFile::handle;

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        int numItem = menuItems.indexOf(menuItem);
        switch (numItem) {
            case 0 :
                new ProjectCreateNew();
                if (Context.isStarted()) {
                    Context.initRCaN();
                    ProjectListsManager.init();
                    MainApplication.updateMenus();
                    ProjectListsManager.addAction("Project creation "+ Context.getFileName());
                    ProjectListsManager.saveExcel();
                    borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
                }
                break;
            case 1:
                new ProjectOpenExisting();
                if (Context.isStarted()) {
                    Context.initRCaN();
                    ProjectListsManager.init();
                    MainApplication.updateMenus();
                    ProjectListsManager.getExcel();
                    borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
                }
                break;
            case 2:
                ProjectListsManager.init();
                Context.setStarted(false);
                MainApplication.updateMenus();
                MainApplication.setFirstPage();
               break;
            case 3:
                if (Context.isStarted()) {
                    if(Context.getDirName().length() == 0){
                        new ProjectSaveAs();
                    }
                    ProjectListsManager.saveExcel();
                }
                break;
            case 4:
                if (Context.isStarted()) {
                    new ProjectSaveAs();
                    ProjectListsManager.saveExcel();
                    ProjectListsManager.addAction("Project saved as "+ Context.getFileName());
                }
                break;
            case 5 :
                MainApplication.exit();
                break;
            default:
        }
    }
}


