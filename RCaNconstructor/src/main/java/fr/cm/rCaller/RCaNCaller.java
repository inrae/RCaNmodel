package fr.cm.rCaller;

import com.github.rcaller.graphics.SkyTheme;
import com.github.rcaller.rstuff.*;
import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.project.ProjectListsManager;
import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;

import java.io.*;
import java.util.List;

public class RCaNCaller {
    static File filePlot;
    static boolean plot = false,  initialized = false,  runOk = false, connected = false;
    static String resultString = "";
    static RCaNScript rCaNScript = null;
    static RCaller caller;
    static RCode code;
    static StringBuilder succ = new StringBuilder("History of connection \n");

    public static void initRCaN(){
        connected = false;
        Context.initRCaN();
        caller = RCaller.create();
        code = RCode.create();
        caller.setRCode(code);
        caller.setGraphicsTheme(new SkyTheme());
        succ.append("Connected to R  \n");
        connected = true;
        Context.setConnectedR(connected);
        Context.setHowConnected(succ.toString());
    }
    // ------------------------------------------------------------------------
    public static void makeRCommand(RCaNScript rCaNScript) {
        RCaNCaller.rCaNScript = rCaNScript;
        rCaNScript.print();
        plot = false;
        filePlot = null;
        resultString = "";
        List<String> commandsR = rCaNScript.getrCompute();
        List<String> commandsP = rCaNScript.getrPlots();
        try {
            code.clear();
            code.addRCode("resultR <- 'ok'");
            if (commandsR.size() > 0) {
                for (String commandLine : commandsR) code.addRCode(commandLine);
            }
            if (commandsP.size() > 0) {
                plot = true;
                filePlot = code.startPlot();
                for (String commandLine : commandsP) code.addRCode(commandLine);
                code.endPlot();
                caller.setRCode(code);
            }
        } catch (Exception ex) {
            new RCaNDialogWarnings("R Interface ", "Problem with R command.", ex);
        }
    }
    // ------------------------------------------------------------------------
    public static void runCommandR()  {
        runOk = false;
        resultString = "";
        try{
            if(plot){
                caller.runAndReturnResultOnline("resultR");
            }
            else {
                caller.runAndReturnResultOnline("resultR", true);
            }
            runOk = true;
        } catch (Exception ex) {
            ex.printStackTrace();
            runOk = false;
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);
            Platform.runLater(() ->{
                new RCaNDialogWarnings("R Interface ","R process has not terminated OK. \n\n" + sw.toString(), ex);
            });
        }
        if(runOk) {
            resultString = RCaNOutputParser.decodeParser( caller, rCaNScript);
        }
    }
    // ------------------------------------------------------------------------
    public static void stopCommandR(){
        try {
            System.out.println("Stop R command");
            caller.stopRCallerAsync();
            caller.deleteTempFiles();
            // caller.stopStreamConsumers();
            // caller.stopRCallerOnline();
        } catch (Exception ex ){
            new RCaNDialogWarnings("R Interface ","R command not stopped properly. \n", ex);
            stopSessionR();
        }
    }
    // ------------------------------------------------------------------------
    static void stopSessionR(){
        try {
            initialized = false;
            Context.initRCaN();
            MainApplication.updateMenus();
            caller.stopStreamConsumers();
            caller.deleteTempFiles();
            caller.stopRCallerAsync();
        } catch (Exception ex ) {
            new RCaNDialogWarnings("R Interface ","R session not stopped properly.", ex);
        }
    }
    // ------------------------------------------------------------------------
    public static HBox getResultsR() {
        if(runOk){
            if( ! rCaNScript.getName().equals("connect")) {
                ProjectListsManager.addTimeLine(rCaNScript.getShortScript(), true);
            }
            rCaNScript.setState(true);
            MainApplication.updateMenus();
            if(rCaNScript.isPlot()){
                try {
                    FileInputStream inputstream = new FileInputStream(filePlot);
                    Image imageR = new Image(inputstream);
                    new RCaNDialogOutput(rCaNScript, imageR);
                }
                catch (FileNotFoundException ex){
                    return(null);
                }
            }
            else if(rCaNScript.isTable()){
                new RCaNDialogOutput(rCaNScript, resultString);
            }
        }
        return(null);
    }
    // ------------------------------------------------------------------------

}

