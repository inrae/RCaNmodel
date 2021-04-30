package fr.cm.rcan;

import com.github.rcaller.exception.ExecutionException;
import com.github.rcaller.graphics.SkyTheme;
import com.github.rcaller.rstuff.*;
import fr.cm.menus.Context;
import fr.cm.menus.MainApplication;
import fr.cm.xml.RCommandXML;
import javafx.scene.layout.HBox;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.logging.Level;
import java.util.logging.Logger;

// import static fr.cm.rcan.RCaNBoxesOutput.RCaNBox;

public class RCaNCallerInterfaceWithR {
    static RCaller caller;
    static RCode code;
    static boolean plot = false;
    static File filePlot;
    static boolean initialized = false;
    static boolean ok = false;
    static String msg = "";
    static String resultString = "";
    static RCommandXML rCommandXML;

    // ------------------------------------------------------------------------
    public static void initR() {
        if( ! initialized) {
            System.out.println("initR");
            Context.initRCaN();
            String OS = System.getProperty("os.name", "unknown").toLowerCase(Locale.ROOT);
            boolean isWindows = OS.contains("win");
            boolean isMac = OS.contains("mac");
            boolean isUnix = OS.contains("nux");

            // initialisation R, RCaller, RCode
            if (isMac) {
                // sur Mac l'appel a RCallerOptions est necessaire
                code = RCode.create();
                RCallerOptions rCallerOptions = RCallerOptions.create(
                        "/usr/local/bin/Rscript",
                        "/usr/local/bin/R", FailurePolicy.CONTINUE,
                        9223372036854775807L,
                        100L,
                        RProcessStartUpOptions.create());
                caller = RCaller.create(code, rCallerOptions);
            }
            if( isUnix || isWindows){
                // sur PC et Linux
                caller = RCaller.create();
                code = RCode.create();
                caller.setRCode(code);
            }
            caller.setGraphicsTheme(new SkyTheme());
            initialized = true;
        }
    }

    public static void makeRCommand(RCommandXML rCommandXML) {
        RCaNCallerInterfaceWithR.rCommandXML = rCommandXML;
        initR();
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
            }
        } catch (Exception e) {
            Logger.getLogger(RCaNDialogForCommand.class.getName()).log(Level.SEVERE, e.getMessage());
            e.printStackTrace();
        }
    }



    // ------------------------------------------------------------------------
    public static void runCommandR()  {
        ok  = false;
        msg ="";
        resultString = "";

        try{
            caller.runAndReturnResultOnline("resultR");
            msg  = "R process has terminated";
            ok = true;
        } catch (ExecutionException ex) {
            ok = false;
            stopCommandR();
            // ex.printStackTrace();
        }
        catch (Exception e){
            msg = "Error in R interface";
            ok = false;
        }
        if(ok) {
            try {
                ROutputParser parser = caller.getParser();
                msg  = "R process has terminated";
                resultString = RCanCallerDecodeResult.decodeParser(rCommandXML,parser);
            } catch (Exception e) {
                Logger.getLogger(RCaNDialogForCommand.class.getName()).log(Level.SEVERE, e.getMessage());
                msg = "R process has terminated. There has been an issue in getting result";
                ok = false;
            }
        }
    }

    // ------------------------------------------------------------------------
    public static void stopCommandR(){
        // System.out.println("stopCommandR");
        try {
            caller.stopStreamConsumers();
            caller.deleteTempFiles();
            caller.stopRCallerOnline();
            msg = "R process stopped by user";
        } catch (ExecutionException ee ){
            stopSessionR();
        }
        catch (RuntimeException re ){
            stopSessionR();
        }
    }

    static void stopSessionR(){
        // System.out.println("stopSessionR");
        initialized = false;
        Context.initRCaN();
        MainApplication.updateMenus();
        caller.stopRCallerAsync();
        msg = "R session stopped by user";
    }



    // ------------------------------------------------------------------------
    public static String getResultString() {
        return resultString;
    }

    public static boolean isOk() {
        return ok;
    }

    public static String getMsg() {
        return msg;
    }

    public static boolean isPlot() {
        return plot;
    }

    public static File getFilePlot() {
        return filePlot;
    }
}

