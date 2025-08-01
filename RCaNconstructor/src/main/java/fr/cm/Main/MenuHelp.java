package fr.cm.Main;


import fr.cm.dialogs.HelpDialog;
import fr.cm.xmlFiles.HelpListXML;
import fr.cm.xmlFiles.HelpXML;
import fr.cm.xmlFiles.RCommandListXML;
import fr.cm.xmlFiles.RCommandXML;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MenuHelp {

    static List<MenuItem> menuItems = null;
    static final Menu what = new Menu("What");
    static final Menu how = new Menu("How");
    static final Menu parameters =  new Menu("Parameters");
    static final Menu rCommands =  new Menu("R Commands");

    static List<Menu> menus = Arrays.asList(what, how, parameters, rCommands);

    static BorderPane borderPaneRacine;

    List<HelpXML> helpsXML = null;
    List<RCommandXML> RCommandXMLS = null;

    public MenuHelp(BorderPane borderPaneRacine) {
        MenuHelp.borderPaneRacine = borderPaneRacine;
        menuItems = new ArrayList<>();
        helpsXML = HelpListXML.getListOfHelpXML();
        for(HelpXML helpXML : helpsXML){
            MenuItem menuItem = new MenuItem(helpXML.getTextMenu());
            switch(helpXML.getSubMenu()){
                case "What":
                    what.getItems().add(menuItem);
                    break;
                case "How":
                    how.getItems().add(menuItem);
                    break;
                case "Coefficient":
                    parameters.getItems().add(menuItem);
                    break;
            }
            menuItems.add(menuItem);
            menuItem.setOnAction(MenuListener);
        }
        RCommandXMLS = RCommandListXML.getListOfRCommandXML();

        for(RCommandXML rCommandXML : RCommandXMLS){
            MenuItem menuItem = new MenuItem(rCommandXML.getTextMenu());
            rCommands.getItems().add(menuItem);
            menuItems.add(menuItem);
            menuItem.setOnAction(MenuListenerR);
        }

    }

    public static List<Menu> getMenus() {
        return menus;
    }

    static final EventHandler<ActionEvent> MenuListener = e -> handle(e);

    static final EventHandler<ActionEvent> MenuListenerR = e -> handleR(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        HelpXML helpXML = HelpListXML.getHelpByTextMenu(menuItem.getText());
        new HelpDialog(helpXML);
    }

    private static void handleR(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        RCommandXML rCommandXML = RCommandListXML.getRCommandByMenu(menuItem.getText());
        new HelpDialog(rCommandXML);
    }

}



