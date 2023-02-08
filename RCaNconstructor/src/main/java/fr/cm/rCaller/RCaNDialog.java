package fr.cm.rCaller;


import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.preferences.ColorsAndFormats;
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

public class RCaNDialog extends Dialog {

    static ExecutorService executor;
    static ThreadCommandR threadCommandR;
    RCommandXML rCommandXML;
    Button goStop = new Button();
    Label caution = new Label();
    Label secondes = new Label();
    boolean commandRunning;
    RTimer rTimer;

    public RCaNDialog(RCommandXML rCommandXML) {
        this.rCommandXML = rCommandXML;
        String stCommandXML = rCommandXML.getStringCommandLine();
        caution.setText("");
        secondes.setText("Not running");;
        goStop.setText("Run R command");
        getParameters(rCommandXML);
        double width = Math.min(500.0, 0.8 * Context.getWindowWidth());
        double height =  Math.min(700.0, 0.8 * Context.getWindowHeight());
        commandRunning = false;

        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCommandXML.getTextMenu());

        Label rHelp = new Label(rCommandXML.getHelp());
        rHelp.setMaxWidth(width);
        rHelp.setStyle(ColorsAndFormats.font);
        rHelp.setWrapText(true);

        Label rCommands = new Label(stCommandXML);
        rCommands.setMaxWidth(width);
        rCommands.setStyle(ColorsAndFormats.font);
        rCommands.setWrapText(true);

        rHelp.setStyle(ColorsAndFormats.font);
        rCommands.setStyle(ColorsAndFormats.font);
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

        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(vbox);
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
        if (!commandRunning) {
            go();
        }
        else {
            stop();
        }
    }
    private  void go() {

        rTimer = new RTimer(this,! rCommandXML.isPlot() && !rCommandXML.isTable(), secondes, caution);
        rTimer.start();
        goStop.setText("Stop R computation");
        caution.setText("If you stop this R computation, results of previous R computations will be lost");
        caution.setVisible(true);
        secondes.setVisible(true);

        executor = Executors.newFixedThreadPool(10);
        threadCommandR = new ThreadCommandR();
        executor.execute(threadCommandR);
        commandRunning = true;
        MainApplication.updateMenus();
    }

    private void stop() {
        executor.shutdownNow();
        RCaNCaller.stopCommandR();
        MainApplication.updateMenus();
        hide();
        close();
        commandRunning = false;
        MainApplication.updateMenus();
    }

    public void disparait(){
        hide();
        close();
    }
    // ------------------------------------------------------------------------
    public class ThreadCommandR implements Runnable {

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


