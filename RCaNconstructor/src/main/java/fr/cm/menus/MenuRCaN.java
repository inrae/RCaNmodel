package fr.cm.menus;

import fr.cm.canObjects.Project;
import fr.cm.rcan.RCaNDialogForCommand;
import fr.cm.xml.RCommandListXML;
import fr.cm.xml.RCommandXML;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class MenuRCaN {

    static List<MenuItem> menuItems = null;
    static final Menu general = new Menu("Environment");
    static final Menu polytope = new Menu("Polytope");
    static final Menu sample =  new Menu("Sample");
    static final Menu analyzePolytope = new Menu("Analyze polytope");
    static final Menu analyzeSample =  new Menu("Analyze sample");
    static final Menu experiments =  new Menu("Experiments");

    static List<MenuItem> menus = Arrays.asList(general, polytope,analyzePolytope, sample, analyzeSample, experiments);

    static BorderPane borderPaneRacine;

    List<RCommandXML> RCommandXMLS = null;

    public MenuRCaN(BorderPane borderPaneRacine) {
        this.borderPaneRacine = borderPaneRacine;
        menuItems = new ArrayList<>();
        RCommandXMLS = RCommandListXML.getListOfRCommandXML();
        for(RCommandXML rCommandXML : RCommandXMLS){
            MenuItem menuItem = new MenuItem(rCommandXML.getTextMenu());
            switch(rCommandXML.getSubMenu()){
                case "general":
                    general.getItems().add(menuItem);
                    break;
                case "polytope":
                    polytope.getItems().add(menuItem);
                    break;
                case "analyzePolytope":
                    analyzePolytope.getItems().add(menuItem);
                    break;
                case "sample":
                    sample.getItems().add(menuItem);
                    break;
                case "analyzeSample":
                    analyzeSample.getItems().add(menuItem);
                    break;
                case "experiments":
                    experiments.getItems().add(menuItem);
                    break;
            }
            menuItems.add(menuItem);
            menuItem.setOnAction(MenuListener);
        }
    }

    static void updateMenus() {
        for(MenuItem menuItem : menuItems){
            RCommandXML rCommandXML = RCommandListXML.getRCommandByMenu(menuItem.getText());
            menuItem.setDisable(! rCommandXML.conditionOK());
            // menuItem.setDisable(false);
        }
    }

    public static List<MenuItem> getMenuItems() {
        return menus;
    }

    static final EventHandler<ActionEvent> MenuListener = e -> handle(e);

    private static void handle(ActionEvent e) {
        MenuItem menuItem = (MenuItem) e.getSource();
        RCommandXML rCommandXML = RCommandListXML.getRCommandByMenu(menuItem.getText());
        RCaNDialogForCommand RCaNDialogForCommand = new RCaNDialogForCommand(rCommandXML);
        HBox hboxResultsR = RCaNDialogForCommand.getResultsR();
        if (hboxResultsR != null) {
            borderPaneRacine.setCenter(hboxResultsR);
        }
        else {
            borderPaneRacine.setCenter(Project.getNetworkView());
        }
    }

}
