package fr.cm.Main;

import fr.cm.project.ProjectCreateNew;
import fr.cm.project.ProjectOpenExisting;
import fr.cm.project.ProjectSaveAs;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.Arrays;
import java.util.List;

public class MenuFile {
    // ---------------------------------
    static final MenuItem newItem = new MenuItem("New"),
            openFileItem = new MenuItem("Open"),
            saveFileItem = new MenuItem("Save"),
            saveAsFileItem = new MenuItem("Save as ..."),
            closeFileItem = new MenuItem("Close"),
            exitItem = new MenuItem("Exit");
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
                    ObjectsManager.init();
                    ObjectsManager.makeMetaElementsList();
                    MainApplication.updateMenus();
                    ObjectsManager.addTimeLine("Project creation "+ Context.getFileName(),true);
                    ObjectsManager.saveExcel();
                    borderPaneRacine.setCenter(ObjectsManager.getNetworkView());
                }
                break;
            case 1:
                new ProjectOpenExisting();
                if (Context.isStarted()) {
                    Context.initRCaN();
                    ObjectsManager.init();
                    MainApplication.updateMenus();
                    ObjectsManager.getExcel();
                    borderPaneRacine.setCenter(ObjectsManager.getNetworkView());
                }
                break;
            case 2:
                ObjectsManager.init();
                Context.setStarted(false);
                MainApplication.updateMenus();
                MainApplication.setFirstPage();
               break;
            case 3:
                if (Context.isStarted()) {
                    if(Context.getDirName().length() == 0){
                        new ProjectSaveAs();
                    }
                    ObjectsManager.saveExcel();
                }
                break;
            case 4:
                if (Context.isStarted()) {
                    new ProjectSaveAs();
                    ObjectsManager.saveExcel();
                    ObjectsManager.addTimeLine("Project saved as "+ Context.getFileName(),true);
                }
                break;
            case 5 :
                MainApplication.exit();
                break;
            default:
        }
    }
}


