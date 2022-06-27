package fr.cm.RCaNMain;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import fr.cm.canObjects.Action;
import fr.cm.canObjects.ProjectListsManager;
import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;

/**
 * @author christianmullon
 */
public class Context {

    private static String fileName; // le nom du fichier projet
    private static String dirName; // le nom du répertoire du fichier projet
    private static File workingDirectory;
    private static int firstYear;
    private static int lastYear;

    // etats du systeme
    private static boolean warnings = false;
    private static boolean changed;
    private static boolean started;
    private static boolean connectedR;
    private static boolean builtR;
    private static boolean sampledR;

    static double windowWidth = 1500.0;
    static double windowHeight = 1000.0;

    static double insideMin = 0.2;
    static double insideMax = 0.8;
    static final double radiusComponent = 30.0f;
    static final double radiusInternalLink = 38.0f;

    // ------------------------------------------------------------------------

    public static boolean isWarnings() { return warnings; }

    public static boolean isChanged() { return changed; }

    public static void setChanged(boolean changed, String comment) {
        Action action = new Action(comment);
        Context.changed = changed;
        ProjectListsManager.addAction(action);
    }

    public static void setChanged(boolean changed) {
        Context.changed = changed;
    }

    public static String getFileName() { return fileName; }

    public static void setFileName(String fileName) { Context.fileName = fileName; }

    public static String getDirName() { return dirName; }

    public static void setDirName(String dirName) { Context.dirName = dirName; }

    public static File getWorkingDirectory() { return workingDirectory; }

    public static String getFullFileName() { return (Context.dirName + "/" + Context.fileName); }

    public static int getFirstYear() { return firstYear; }

    public static int getLastYear() { return lastYear; }

    public static void setFirstYear(int firstYear) { Context.firstYear = firstYear; }

    public static void setLastYear(int lastYear) { Context.lastYear = lastYear; }

    public static int getNbYears() { return lastYear - firstYear + 1; }

    public static boolean isConnectedR() { return connectedR; }

    public static void setConnectedR(boolean connectedR) { Context.connectedR = connectedR; }

    public static boolean isBuiltR() { return builtR; }

    public static void setBuiltR(boolean builtR) { Context.builtR = builtR; }

    public static boolean isSampledR() { return sampledR; }

    public static void setSampledR(boolean sampledR) { Context.sampledR = sampledR; }

    public static boolean isStarted() { return started; }

    public static void setStarted(boolean started) { Context.started = started; }

    public static double getWindowWidth() { return windowWidth; }

    public static void setWindowWidth(double w) { windowWidth = w; }

    public static double getWindowHeight() { return windowHeight; }

    public static void setWindowHeight(double h) { windowHeight = h; }

    public static double getInsideMin() { return insideMin; }

    public static double getInsideMax() { return insideMax; }

    public static double getBackgroundWidth() { return windowWidth; }

    public static double getBackgroundHeight() { return (0.9 * windowHeight); }

    public static double getRadiusComponent() { return radiusComponent; }

    public static double getRadiusInternalLink() { return radiusInternalLink; }

    public static String getDate(){
        Date date = new Date();
        SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
        return(formatter.format(date));
    }
    // ----------------------------------------------------------------------------------
    public static List<String> getObservationsYears() {
        List<String> liste = new ArrayList<>();
        for (int y = getFirstYear(); y <= getLastYear(); y++) {
            liste.add(Integer.toString(y));
        }
        return (liste);
    }

    public static void init() {
        firstYear = 4000;
        lastYear = -1000;
        workingDirectory = new File(System.getProperty("user.dir"));
        started = false;
        initRCaN();
        Rectangle2D rect = Screen.getPrimary().getVisualBounds();
        double screenWidth = rect.getWidth();
        double screenHeight = rect.getHeight();
        setWindowWidth(0.9 * screenWidth);
        setWindowHeight(0.9 * screenHeight);
        changed = false;
    }

    public static void initRCaN() {
        connectedR = false;
        builtR = false;
        sampledR = false;
    }
}
