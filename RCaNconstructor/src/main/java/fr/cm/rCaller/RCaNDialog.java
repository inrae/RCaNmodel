package fr.cm.rCaller;


import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import fr.cm.parameters.ColorsAndFormats;
import fr.cm.xmlFiles.RCommandXML;
import javafx.application.Platform;
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
        rTimer = new RTimer();
        rTimer.start();
        started = false;
        this.rCommandXML = rCommandXML;
        getParameters(rCommandXML);
        RCaNCaller.initR();
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
        if(goStop.getText().equals("OK")){
            hide();
            close();
        }
        // si ce n'est pas demarre, on demarre
        else if (! started) {
            rTimer.setStarted(true);
            rTimer.setLabel(secondes);
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
            rTimer.setStarted(false);
            executor.shutdownNow();
            RCaNCaller.stopCommandR();
            MainApplication.updateMenus();
            hide();
            close();
            started = false;
            MainApplication.updateMenus();
        }
    }

    // ------------------------------------------------------------------------
    public  class ThreadCommandR implements Runnable {
        public void run()  {
            if(rCommandXML.isDisconnect()){
                RCaNCaller.stopSessionR();
            } else {
                RCaNCaller.runCommandR();
            }
            rTimer.setStarted(false);
            manageDialog();
        }
    }

    void manageDialog(){
        Platform.runLater(() -> {
            if(rCommandXML.isPlot() || rCommandXML.isTable()) {
                hide();
                close();
            } else {
                goStop.setText("OK");
                secondes.setText("");
                caution.setText(RCaNCaller.resultString);
            }

        });
    }
    // ------------------------------------------------------------------------
}


