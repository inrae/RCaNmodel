package fr.cm.rCaller;

import com.github.rcaller.rstuff.*;
import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.project.ProjectListsManager;
import fr.cm.xmlFiles.RCommandXML;
import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;

import java.io.*;
import java.util.List;

public class RCaNCaller {
    static File filePlot;
    static boolean plot = false,  initialized = false,  runOk = false;
    static String resultString = "";
    static RCommandXML rCommandXML = null;
    static RCaller caller;
    static RCode code;

    // ------------------------------------------------------------------------
    public static void makeRCommand(RCommandXML rCommandXML) {
        caller = RCaNStartR.getCaller();
        code = RCaNStartR.getCode();
        RCaNCaller.rCommandXML = rCommandXML;
        plot = false;
        filePlot = null;
        resultString = "";
        List<String> commandsR = rCommandXML.getrCompute();
        List<String> commandsP = rCommandXML.getrPlots();
        try {
            code.clear();
            code.addRCode("resultR <- 'ok'");
            if (commandsR.size() > 0) {
                for (String commandLine : commandsR) {
                    code.addRCode(rCommandXML.explicitCommandLine( commandLine));
                }
            }
            if (commandsP.size() > 0) {
                plot = true;
                filePlot = code.startPlot();
                for (String commandLine : commandsP) {
                    code.addRCode(rCommandXML.explicitCommandLine( commandLine));
                }
                code.endPlot();
                caller.setRCode(code);
            }
        } catch (Exception ex) {
            new RCaNDialogWarnings("R Interface ", "Problem with R command.", ex);
        }
    }
    // ------------------------------------------------------------------------
    public static void runCommandR()  {
        caller = RCaNStartR.getCaller();
        code = RCaNStartR.getCode();
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
            resultString = RCaNParser.decodeParser( caller,  rCommandXML);
        }
    }
    // ------------------------------------------------------------------------
    public static void stopCommandR(){
        RCaller caller = RCaNStartR.getCaller();
        RCode code = RCaNStartR.getCode();
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
        RCaller caller = RCaNStartR.getCaller();
        RCode code = RCaNStartR.getCode();
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
            if( ! rCommandXML.getName().equals("connect")) {
                ProjectListsManager.addTimeLine(rCommandXML.getShortStringCommandLine(), true);
            }
            rCommandXML.setState(true);
            MainApplication.updateMenus();
            if(rCommandXML.isPlot()){
                try {
                    FileInputStream inputstream = new FileInputStream(filePlot);
                    Image imageR = new Image(inputstream);
                    new RCaNDialogOutput(rCommandXML, imageR);
                }
                catch (FileNotFoundException ex){
                    return(null);
                }
            }
            else if(rCommandXML.isTable()){
                new RCaNDialogOutput(rCommandXML, resultString);
            }
        }
        return(null);
    }
    // ------------------------------------------------------------------------

}

