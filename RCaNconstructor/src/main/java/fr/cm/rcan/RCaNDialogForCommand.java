package fr.cm.rcan;


import fr.cm.menus.Context;
import fr.cm.menus.MainApplication;
import fr.cm.xml.RCommandXML;
import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.stage.Window;

import java.io.File;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class RCaNDialogForCommand extends Dialog {

    static ExecutorService executor;
    static ThreadCommandR threadCommandR;
    RCommandXML rCommandXML;

    Button goStop = new Button("Start");
    Label caution = new Label("If you stop this R computation, results of previous R computations will be lost");


    public RCaNDialogForCommand(RCommandXML rCommandXML) {
        this.rCommandXML = rCommandXML;
        getParameters(rCommandXML);
        RCaNCallerInterfaceWithR.initR();
        double width = 0.6 * Context.getWidth();
        double height =  0.7 *Context.getHeight();

        String stCommandXML = rCommandXML.getSt();
        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCommandXML.getTextMenu());

        ScrollPane scrollPane = new ScrollPane();

        Label rHelp = new Label(rCommandXML.getHelp());
        rHelp.setMaxWidth(width);
        rHelp.setStyle("-fx-background-color: white;");
        rHelp.setWrapText(true);

        Label rCommands = new Label(stCommandXML);
        rCommands.setMaxWidth(width);
        rCommands.setStyle("-fx-background-color: white;");
        rCommands.setWrapText(true);

        goStop.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                goStopR();
            }
        });

        caution.setVisible(false);

        VBox vbox = new VBox();
        vbox.setSpacing(10);
        vbox.setPadding(new Insets(10, 10, 10, 10));
        vbox.setAlignment(Pos.CENTER);
        vbox.getChildren().addAll(rCommands, goStop, caution);


        scrollPane.setContent(vbox);
        scrollPane.setFitToHeight(true);
        scrollPane.setFitToWidth(true);
        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(scrollPane);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        Node closeButton = getDialogPane().lookupButton(ButtonType.CLOSE);
        closeButton.setVisible(false);
        showAndWait();
    }

    // ------------------------------------------------------------------------
    public  void getParameters(RCommandXML rCommandXML) {
        if (rCommandXML.getName().equals("sample")) {
            new RCaNDialogGetParametersForSampling(rCommandXML);
        } else {
            new RCaNDialogGetParametersForRCommand(rCommandXML);
        }
        RCaNCallerInterfaceWithR.makeRCommand(rCommandXML);
    }

    // ------------------------------------------------------------------------
    public  HBox getResultsR() {
        String msg = RCaNCallerInterfaceWithR.getMsg();
        String resultString = RCaNCallerInterfaceWithR.getResultString();
        File filePlot = RCaNCallerInterfaceWithR.getFilePlot();
        boolean rOk = RCaNCallerInterfaceWithR.isOk();
        boolean plot = RCaNCallerInterfaceWithR.isPlot();
        boolean table = rCommandXML.isTable();

        String messageCommande = rCommandXML.getTextMenu() + " : " + msg;
        MainApplication.getAnAnimationTimer().setMessage(messageCommande);
        if(rOk){
            rCommandXML.setState(true);
            MainApplication.updateMenus();
            if(plot){
                return RCaNBoxesOutput.RCaNBox(filePlot);
            }
            if(table){
                return RCaNBoxesOutput.RCaNBox(resultString);
            }
        }
        return(null);
    }

    boolean started = false;
    private  void goStopR() {
        if (! started) {
            MainApplication.getAnAnimationTimer().setStarted(true);
            executor = Executors.newFixedThreadPool(10);
            threadCommandR = new ThreadCommandR();
            executor.execute(threadCommandR);
            goStop.setText("Stop");
            caution.setVisible(true);
            started = true;
        } else {
            MainApplication.getAnAnimationTimer().setStarted(false);
            RCaNCallerInterfaceWithR.stopCommandR();
            executor.shutdownNow();
            MainApplication.updateMenus();
            closeDialog();
         }
    }

    // ------------------------------------------------------------------------
    public  class ThreadCommandR implements Runnable {
        @Override
        public void run()  {
            RCaNCallerInterfaceWithR.runCommandR();
            MainApplication.getAnAnimationTimer().setStarted(false);
            closeDialog();
        }
    }

    void closeDialog(){
        Platform.runLater(() -> {
            hide();
            close();
        });

    }
    // ------------------------------------------------------------------------
}

