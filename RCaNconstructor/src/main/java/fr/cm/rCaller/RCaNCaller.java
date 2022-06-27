package fr.cm.rCaller;

import com.github.rcaller.graphics.SkyTheme;
import com.github.rcaller.rstuff.*;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import fr.cm.xmlFiles.RCommandXML;
import javafx.application.Platform;
import javafx.scene.image.Image;
import javafx.scene.layout.HBox;

import java.io.*;
import java.util.List;

public class RCaNCaller {
    static RCaller caller;
    static RCode code;
    static File filePlot;
    static boolean plot = false,  initialized = false,  runOk = false;
    static String resultString = "";
    static RCommandXML rCommandXML = null;

    // ------------------------------------------------------------------------
    public static void initRCaller() {
        if( ! initialized) {
            Context.initRCaN();
            caller = RCaller.create();
            code = RCode.create();
            caller.setRCode(code);
            caller.setGraphicsTheme(new SkyTheme());
            check("standard R and RScript");
            if(! initialized) {
                initRdir("/usr/local/bin/");
                check("/usr/local/bin/");
            }
            if(! initialized) {
                initRdir("/Applications/anaconda3/bin/");
                check("/Applications/anaconda3/bin/");
            }
            if(! initialized) {
                System.exit(0);
            }
        }
    }
    // ------------------------------------------------------------------------
    public static void initRdir(String dir) {
        code = RCode.create();
        RCallerOptions rCallerOptions = RCallerOptions.create(
                dir + "Rscript",
                dir + "R",
                FailurePolicy.CONTINUE,
                9223372036854775807L,
                100L,
                RProcessStartUpOptions.create()
        );
        try {
            caller = RCaller.create(code, rCallerOptions);
        }
        catch(Exception ex){
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);
            new RCaNInterfaceDialog("R Interface ", "Unable to create code and caller", ex);
            initialized = false;
        }
    }
    // ------------------------------------------------------------------------
    static void check(String message){
        try{
            code.addRCode("test <- 0");
            caller.runAndReturnResult("test");
            caller.getParser();
            initialized = true;
            HelpDialog.warning("R Interface ","Connection to R initialized on " +message + "\n");
        } catch(Exception ex){
            new RCaNInterfaceDialog("R Interface ","Unable to initialize connection to R on "
                    + message , ex);
            initialized = false;
        }
     }
    // ------------------------------------------------------------------------
    public static void makeRCommand(RCommandXML rCommandXML) {
        RCaNCaller.rCommandXML = rCommandXML;
        initRCaller();
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
                    code.addRCode(rCommandXML.completeCommandLine( commandLine));
                }
            }
            if (commandsP.size() > 0) {
                plot = true;
                filePlot = code.startPlot();
                for (String commandLine : commandsP) {
                    code.addRCode(rCommandXML.completeCommandLine( commandLine));
                }
                code.endPlot();
                caller.setRCode(code);
                Context.setChanged(false, rCommandXML.actionCommandLine());
            }
         } catch (Exception ex) {
            new RCaNInterfaceDialog("R Interface ", "Problem with R command.", ex);
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
            // stopCommandR();
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);
            Platform.runLater(() ->{
                new RCaNInterfaceDialog("R Interface ","R process has not terminated OK. \n\n" + sw.toString(), ex);
            });
         }
        if(runOk) {
            resultString = RCaNParser.decodeParser( caller,  rCommandXML);
         }
        Context.setChanged(false, "Result : "+ runOk);
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
            new RCaNInterfaceDialog("R Interface ","R command not stopped properly. \n", ex);
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
            new RCaNInterfaceDialog("R Interface ","R session not stopped properly.", ex);
        }
    }
    // ------------------------------------------------------------------------
    public static HBox getResultsR() {
        if(runOk){
            rCommandXML.setState(true);
            MainApplication.updateMenus();
            if(rCommandXML.isPlot()){
                try {
                    FileInputStream inputstream = new FileInputStream(filePlot);
                    Image imageR = new Image(inputstream);
                    return RCaNBoxesOutput.RCaNBox(imageR);
                }
                catch (FileNotFoundException ex){
                    return(null);
                }
            }
            else if(rCommandXML.isTable()){
                return RCaNBoxesOutput.RCaNBox(resultString);
            }
        }
        return(null);
    }
    // ------------------------------------------------------------------------
}

