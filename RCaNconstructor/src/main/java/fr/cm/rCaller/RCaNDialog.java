package fr.cm.rCaller;


import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.preferences.ColorsAndFormats;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;
import javafx.stage.Window;

public class RCaNDialog extends Dialog {

    RCaNScript rCaNScript;
    Button goStop = new Button();
    Label caution = new Label();
    boolean commandRunning  = false;
    public RCaNDialog(RCaNScript rCaNScript) {
        // --------------------------------------
        this.rCaNScript = rCaNScript;
        this.rCaNScript.setScriptParameters();
        RCaNCaller.makeRCommand(rCaNScript);
        // --------------------------------------
        caution.setText("");
        goStop.setText("Run R command");
        double width = Math.min(500.0, 0.8 * Context.getWindowWidth());
        double height =  Math.min(700.0, 0.8 * Context.getWindowHeight());
         // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCaNScript.getTextMenu());

        Label rHelp = new Label(rCaNScript.getHelp());
        rHelp.setMaxWidth(width);
        rHelp.setStyle(ColorsAndFormats.font);
        rHelp.setWrapText(true);

        Label rCommands = new Label(rCaNScript.getScriptString());
        rCommands.setMaxWidth(width);
        rCommands.setStyle(ColorsAndFormats.font);
        rCommands.setWrapText(true);

        rHelp.setStyle(ColorsAndFormats.font);
        rCommands.setStyle(ColorsAndFormats.font);
        goStop.setStyle(ColorsAndFormats.font);
        caution.setStyle(ColorsAndFormats.font);

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

        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(vbox);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        Node closeButton = getDialogPane().lookupButton(ButtonType.CLOSE);
        closeButton.setVisible(false);
    }

    // --------------------------------------------
    private  void goStopR() {
        if (!commandRunning) go();
        else stop();
        }
    private  void go() {
        goStop.setText("Stop R computation");
        caution.setText("If you stop this R computation, results of previous R computations will be lost");
        caution.setVisible(true);
        commandRunning = true;
        RCaNCaller.runCommandR();
        commandRunning = false;
        clearDialog();
        MainApplication.updateMenus();
    }

    private void stop() {
        RCaNCaller.stopCommandR();
        commandRunning = false;
        clearDialog();
        MainApplication.updateMenus();
    }

    public void clearDialog(){
        hide();
        close();
    }
    // ------------------------------------------------------------------------
}


