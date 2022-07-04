package fr.cm.rCaller;


import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import fr.cm.parameters.ColorsAndFormats;
import fr.cm.xmlFiles.RCommandXML;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Window;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static java.lang.Thread.sleep;

public class RCaNDialog extends Dialog {

    static ExecutorService executor;
    static ThreadCommandR threadCommandR;
    RCommandXML rCommandXML;

    Button goStop = new Button("Start");
    Label caution = new Label("If you stop this R computation, results of previous R computations will be lost");
    Label secondes = new Label("Not started");
    boolean started;
    RTimer rTimer;

    public RCaNDialog(RCommandXML rCommandXML) {
        started = false;
        Label caution = new Label("");
        secondes = new Label("Not started");
        this.rCommandXML = rCommandXML;
        getParameters(rCommandXML);
        RCaNCaller.initRCaller();
        double width = 0.6 * Context.getWindowWidth();
        double height =  0.7 * Context.getWindowHeight();

        String stCommandXML = rCommandXML.getSt();
        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCommandXML.getTextMenu());

        ScrollPane scrollPane = new ScrollPane();

        Label rHelp = new Label(rCommandXML.getHelp());
        rHelp.setMaxWidth(width);
        rHelp.setStyle(ColorsAndFormats.font);
        rHelp.setWrapText(true);

        Label rCommands = new Label(stCommandXML);
        rCommands.setMaxWidth(width);
        rCommands.setStyle(ColorsAndFormats.font);
        rCommands.setWrapText(true);

        secondes.setStyle(ColorsAndFormats.font);
        goStop.setStyle(ColorsAndFormats.font);
        caution.setStyle(ColorsAndFormats.font);

        goStop.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {

                goStopR();
            }
        });

        caution.setVisible(false);
        secondes.setVisible(true);

        VBox vbox = new VBox();
        vbox.setSpacing(10);
        vbox.setPadding(new Insets(10, 10, 10, 10));
        vbox.setAlignment(Pos.CENTER);
        vbox.getChildren().addAll(rCommands, goStop, caution, secondes);

        scrollPane.setContent(vbox);
        scrollPane.setFitToHeight(true);
        scrollPane.setFitToWidth(true);
        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(scrollPane);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        Node closeButton = getDialogPane().lookupButton(ButtonType.CLOSE);
        closeButton.setVisible(false);
      }

    // ------------------------------------------------------------------------
    public  void getParameters(RCommandXML rCommandXML) {
        if (rCommandXML.getName().equals("sample")) {
            new RCaNDialogGetParametersForSampling(rCommandXML);
        } else {
            new RCaNDialogGetParametersForRCommand(rCommandXML);
        }
        RCaNCaller.makeRCommand(rCommandXML);
    }

    // --------------------------------------------

    private  void goStopR() {
        // si ce n'est pas demarre, on demarre
        if (! started) {
            caution = new Label("If you stop this R computation, results of previous R computations will be lost");
            rTimer = new RTimer(this,! rCommandXML.isPlot() && !rCommandXML.isTable(), secondes, caution, goStop);
            rTimer.start();
            executor = Executors.newFixedThreadPool(10);
            threadCommandR = new ThreadCommandR();
            executor.execute(threadCommandR);
            goStop.setText("Stop");
            caution.setVisible(true);
            secondes.setVisible(true);
            started = true;
            MainApplication.updateMenus();
        }
        // s c'est demarre, on arrete
        else {
            executor.shutdownNow();
            RCaNCaller.stopCommandR();
            MainApplication.updateMenus();
            hide();
            close();
            started = false;
            MainApplication.updateMenus();
        }
    }

    public void disparait(){
        hide();
        close();
    }
    // ------------------------------------------------------------------------
    public  class ThreadCommandR implements Runnable {

        public void run()  {
            if(rCommandXML.isDisconnect()){
                RCaNCaller.stopSessionR();
            } else {
                RCaNCaller.runCommandR();
                rTimer.setStringResult(RCaNCaller.resultString);
                rTimer.setCompleted(true);
            }
            };
        }

    // ------------------------------------------------------------------------
}


