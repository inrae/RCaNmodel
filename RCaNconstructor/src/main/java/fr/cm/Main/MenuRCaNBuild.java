package fr.cm.Main;

import fr.cm.dialogs.HelpDialog;
import fr.cm.project.ProjectListsManager;
import fr.cm.rCaller.RCaNDialog;
import fr.cm.rCaller.RCaNCaller;
import fr.cm.xmlFiles.RCommandListXML;
import fr.cm.rCaller.RCaNScript;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;

import java.util.ArrayList;
import java.util.List;

public class MenuRCaNBuild {

    static List<MenuItem> menuItems = null;
    static BorderPane borderPaneRacine;


    public MenuRCaNBuild(BorderPane borderPaneRacine) {
        MenuRCaNBuild.borderPaneRacine = borderPaneRacine;
        menuItems = new ArrayList<>();
        for (RCaNScript rCaNScriptXML : RCommandListXML.getListOfRCommandXML()) {
            if (rCaNScriptXML.getMenu().equals("build")) {
                MenuItem menuItem = new MenuItem(rCaNScriptXML.getTextMenu());
                menuItems.add(menuItem);
                menuItem.setOnAction(MenuListener);
            }
        }
    }

    static void updateMenus() {
        boolean notStarted = (!Context.isStarted()) && (!Context.isConnectedR());
        for (MenuItem menuItem : menuItems) {
            if (notStarted) menuItem.setDisable(notStarted);
            else {
                RCaNScript rCaNScriptXML = RCommandListXML.getRCommandByMenu(menuItem.getText());
                menuItem.setDisable(!rCaNScriptXML.conditionOK());
            }
        }
    }

    public static List<MenuItem> getMenuItems() {

        return menuItems;
    }

    static final EventHandler<ActionEvent> StatusListener = e -> statusConnection(e);

    private static void statusConnection(ActionEvent e) {
        String howConnected = Context.getHowConnected();
        HelpDialog.warning(howConnected, "Connection with R");
    }

    static final EventHandler<ActionEvent> MenuListener = e -> handle(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        RCaNScript rCaNScript = RCommandListXML.getRCommandByMenu(menuItem.getText());
        new RCaNDialog(rCaNScript).showAndWait();
        HBox hboxResultsR = RCaNCaller.getResultsR();
        if (hboxResultsR != null) {
            borderPaneRacine.setCenter(hboxResultsR);
        }
        else {
            borderPaneRacine.setCenter(ProjectListsManager.getNetworkView());
        }
    }

}
