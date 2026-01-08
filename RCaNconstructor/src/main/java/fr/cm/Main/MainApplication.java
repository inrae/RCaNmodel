
package fr.cm.Main;

import fr.cm.preferences.ColorsAndFormats;
import fr.cm.rCaller.RCaNCaller;
import fr.cm.xmlFiles.HelpListXML;
import fr.cm.xmlFiles.RCommandListXML;
import javafx.application.Application;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

import java.io.FileWriter;
import java.util.Timer;
import java.util.TimerTask;


/**
 * @author Christian Mullon
 *
 */
public class MainApplication extends Application {

    public static void main(String[] args) {
        launch(args);
    }

    public static Stage stage;
    static BorderPane borderPaneRacine;
    static FirstPage firstPage;
    FileWriter logWriter;
    @Override
    public void start(Stage primaryStage) {
        RCaNCaller.initRCaN();
        Logg.createLog();
        stage = primaryStage;
        stage.widthProperty().addListener(changeSizelistener);
        stage.heightProperty().addListener(changeSizelistener);
        setOnClose();
        HelpListXML.init();
        RCommandListXML.init();
        Context.init();
        ObjectsManager.init();
        ObjectsManager.getNetworkView().update();
        borderPaneRacine = new BorderPane();

        setFirstPage();
        MenuBar menuBar = setMenus();
        borderPaneRacine.setTop(menuBar);

        Scene scene = new Scene(borderPaneRacine, Context.getWindowWidth(), Context.getWindowHeight());
        scene.getRoot().setStyle(ColorsAndFormats.font);

        primaryStage.setX(0.05 * Context.getWindowWidth());
        primaryStage.setY(0.05 * Context.getWindowHeight());
        primaryStage.setScene(scene);
        primaryStage.show();
    }


    static void setFirstPage(){
        firstPage = new FirstPage();
        borderPaneRacine.setCenter(firstPage);
    }
    public static void updateMenus() {
        MenuFile.updateMenus();
        MenuTrophicNetwork.updateMenus();
        MenuDocument.updateMenus();
        MenuRCaNBuild.updateMenus();
        MenuRCaNSample.updateMenus();
    }

    static MenuBar setMenus() {
        new MenuFile(borderPaneRacine);
        new MenuRCaNBuild(borderPaneRacine);
        new MenuRCaNSample(borderPaneRacine);
        new MenuHelp(borderPaneRacine);
        new MenuTrophicNetwork(borderPaneRacine);
        new MenuDocument(borderPaneRacine);

        MenuBar menuBar = new MenuBar();
        Menu fileMenu = new Menu("File");
        Menu viewMenu = new Menu("Set up trophic network");
        Menu documentMenu = new Menu("Document project");
        Menu rcanMenuBuild = new Menu("Build polytope");
        Menu rcanMenuSample = new Menu("Sample trajectories");
        Menu helpMenu = new Menu("Information and help");

        fileMenu.getItems().addAll(MenuFile.getMenuItems());
        viewMenu.getItems().addAll(MenuTrophicNetwork.getMenuItems());
        documentMenu.getItems().addAll(MenuDocument.getMenuItems());
        rcanMenuBuild.getItems().addAll(MenuRCaNBuild.getMenuItems());
        rcanMenuSample.getItems().addAll(MenuRCaNSample.getMenuItems());
        helpMenu.getItems().addAll(MenuHelp.getMenus());

        menuBar.getMenus().addAll(fileMenu, viewMenu, rcanMenuBuild, rcanMenuSample, documentMenu, helpMenu);
        updateMenus();
        return menuBar;
    }
    public static void exit() {
        Platform.exit();
        System.exit(0);
    }
    static void setOnClose() {
        Logg.addLog("Exit");
        stage.setOnCloseRequest((WindowEvent t) -> exit());
    }
    // -------------------------------------------------------------------------------------
    final ChangeListener<Number> changeSizelistener = new ChangeListener<>() {
        // on observe si l'utilisateur est en train de changer la taille de la fenetre principale
        final Timer timer = new Timer();
        TimerTask task = null;
        final long delayTime = 100;
        @Override
        public void changed(ObservableValue<? extends Number> observable, Number oldValue, final Number newValue) {
            if (task != null) {
                task.cancel();
            }
            task = new TimerTask() {
                @Override
                public void run() {
                    double nw = MainApplication.stage.getWidth();
                    double nh = MainApplication.stage.getHeight();
                    Context.setWindowWidth(nw);
                    Context.setWindowHeight(nh);
                    if (nw > 1.0 && nh > 1.0) {
                           Platform.runLater(() -> ObjectsManager.getNetworkView().redrawChangingSize());
                    }
                }
            };
            timer.schedule(task, delayTime);
        }
    };
    // ------------------------------------------------------------------------
    public static void setTitle(String fileName) {
        stage.setTitle(" RCaN - " + fileName.replace(".xlsx", " "));
    }
    public static Stage getStage() {
        return stage;
    }

}

