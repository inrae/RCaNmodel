
package fr.cm.menus;

import fr.cm.dialogs.*;
import fr.cm.xml.HelpListXML;
import fr.cm.xml.RCommandListXML;
import javafx.animation.AnimationTimer;
import javafx.application.Application;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.stage.WindowEvent;

import fr.cm.canObjects.Project;

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
    Label bottom = new Label("bottom");
    static AnAnimationTimer anAnimationTimer;

    @Override
    public void start(Stage primaryStage) {
        anAnimationTimer = new AnAnimationTimer();
        anAnimationTimer.start();

        stage = primaryStage;
        stage.widthProperty().addListener(changeSizelistener);
        stage.heightProperty().addListener(changeSizelistener);

        RCommandListXML.init();
        HelpListXML.init();
        Context.init();
        Project.init();

        Project.getNetworkView().update();

        borderPaneRacine = new BorderPane();

        setOnClose();

        MenuBar menuBar = setMenus();

        firstPage = new FirstPage();
        borderPaneRacine.setCenter(firstPage);
        borderPaneRacine.setBottom(bottom);
        borderPaneRacine.setTop(menuBar);

        Scene scene = new Scene(borderPaneRacine, Context.getWidth(), Context.getHeight());
        primaryStage.setX(0.05 * Context.getWidth());
        primaryStage.setY(0.05 * Context.getHeight());
        primaryStage.setScene(scene);
        primaryStage.show();
    }

    // -------------------------------------------------------------------------------------
    final ChangeListener<Number> changeSizelistener = new ChangeListener<>() {
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
                    Platform.runLater(() -> {
                        Project.getNetworkView().redrawChangingSize();
                    });
                }
            };
            timer.schedule(task, delayTime);
        }
    };

    public static void setTitle(String fileName) {
        stage.setTitle(" RCaN - " + fileName.replace(".xlsx", " "));
    }

    public static Stage getStage() {
        return stage;
    }

    public static void updateMenus() {
        MenuFile.updateMenus();
        MenuMeta.updateMenus();
        MenuView.updateMenus();
        MenuRCaN.updateMenus();
    }

    static MenuBar setMenus() {
        new MenuRCaN(borderPaneRacine);
        new MenuMeta(borderPaneRacine);
        new MenuFile(borderPaneRacine);
        new MenuHelp(borderPaneRacine);
        new MenuView(borderPaneRacine);

        MenuBar menuBar = new MenuBar();
        Menu fileMenu = new Menu("File");
        Menu metaMenu = new Menu("Meta Information");
        Menu viewMenu = new Menu("View");
        Menu rcanMenu = new Menu("RCaN");
        Menu helpMenu = new Menu("Information and help");

        fileMenu.getItems().addAll(MenuFile.getMenuItems());
        metaMenu.getItems().addAll(MenuMeta.getMenuItems());
        viewMenu.getItems().addAll(MenuView.getMenuItems());
        rcanMenu.getItems().addAll(MenuRCaN.getMenuItems());
        helpMenu.getItems().addAll(MenuHelp.getMenus());

        menuBar.getMenus().addAll(fileMenu, metaMenu, viewMenu, rcanMenu, helpMenu);
        updateMenus();
        return menuBar;
    }

    public static void close() {
        boolean answer = true;
        if (Context.isChanged()) {
            answer = new ProjectSaveBeforeClosing().answer;
        }
        if (answer) {
            Platform.exit();
            System.exit(0);
        }
    }

    static void setOnClose() {
        stage.setOnCloseRequest((WindowEvent t) -> {
            close();
        });
    }

    // ------------------------------------------------------------------------
    public static AnAnimationTimer getAnAnimationTimer() {
        return anAnimationTimer;
    }
    public  class AnAnimationTimer extends AnimationTimer {
        boolean started = false;
        int seconds;
        String message = "";
        public void setMessage(String ms){
            bottom.setFont(Font.font ("Verdana", 20));
            bottom.setTextFill(Color.RED);
            seconds = 0;
            message = ms;
        }
        public void setStarted(boolean st){
            bottom.setFont(Font.font ("Verdana", 20));
            bottom.setTextFill(Color.RED);
            seconds = 0;
            started = st;
        }
        long sleeps = 1000000000;
        long prevTime = 0;
        @Override
        public void handle(long now) {
            if((now-prevTime)<sleeps){
                return;
            }
            prevTime = now;
            doHandle();
        }
        void doHandle(){
            seconds++;
            if(started) {
                bottom.setText("             Running RCaN command. Elapsed time : "
                        + seconds + " seconds");
            }
            else {
                if(seconds>10){
                    message = "";
                }
                bottom.setText("             " + message);
            }
        }
    }
}

