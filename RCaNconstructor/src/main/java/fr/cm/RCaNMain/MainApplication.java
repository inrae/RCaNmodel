
package fr.cm.RCaNMain;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
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

    @Override
    public void start(Stage primaryStage) {

        RCaNCaller.initRCaller();

        stage = primaryStage;
        stage.widthProperty().addListener(changeSizelistener);
        stage.heightProperty().addListener(changeSizelistener);
        setOnClose();

        RCommandListXML.init();
        HelpListXML.init();
        Context.init();
        ProjectListsManager.init();
        ProjectListsManager.getNetworkView().update();

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
        MenuView.updateMenus();
        MenuRCaN.updateMenus();
    }

    static MenuBar setMenus() {
        new MenuRCaN(borderPaneRacine);
        new MenuFile(borderPaneRacine);
        new MenuHelp(borderPaneRacine);
        new MenuView(borderPaneRacine);

        MenuBar menuBar = new MenuBar();
        Menu fileMenu = new Menu("File");
        Menu viewMenu = new Menu("View");
        Menu rcanMenu = new Menu("RCaN");
        Menu helpMenu = new Menu("Information and help");

        fileMenu.getItems().addAll(MenuFile.getMenuItems());
        viewMenu.getItems().addAll(MenuView.getMenuItems());
        rcanMenu.getItems().addAll(MenuRCaN.getMenuItems());
        helpMenu.getItems().addAll(MenuHelp.getMenus());

        menuBar.getMenus().addAll(fileMenu, viewMenu, rcanMenu, helpMenu);
        updateMenus();
        return menuBar;
    }

    public static void exit() {
        Platform.exit();
        System.exit(0);
     }

    static void setOnClose() {

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
                    Platform.runLater(() -> ProjectListsManager.getNetworkView().redrawChangingSize());
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

