package fr.cm.menus;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import javafx.geometry.Rectangle2D;
import javafx.stage.Screen;

/**
 * @author christianmullon
 */
public class Context {

    private static String fileName; // le nom du fichier projet
    private static String dirName; // le nom du répertoire du fichier projet
    private static File workingDirectory;
    private static int minYear;
    private static int maxYear;

    // etats du systeme
    private static boolean changed;
    private static boolean started;
    private static boolean RCanInstalled;
    private static boolean connectedR;
    private static boolean builtR;
    private static boolean sampledR;


    static double width = 1500.0;
    static double height = 1000.0;
    static double insideMin = 0.2;
    static double insideMax = 0.8;
    static final double radiusComponent = 30.0f;
    static final double radiusInternalLink = 38.0f;

    // ------------------------------------------------------------------------

    public static boolean isRCanInstalled() { return RCanInstalled; }

    public static void setRCanInstalled(boolean RCanInstalled) { Context.RCanInstalled = RCanInstalled; }

    public static boolean isChanged() { return changed; }

    public static void setChanged(boolean changed) { Context.changed = changed; }

    public static String getFileName() { return fileName; }

    public static void setFileName(String fileName) { Context.fileName = fileName; }

    public static String getDirName() { return dirName; }

    public static void setDirName(String dirName) { Context.dirName = dirName; }

    public static File getWorkingDirectory() { return workingDirectory; }

    public static String getFullFileName() { return (Context.dirName + "/" + Context.fileName); }

    public static int getMinYear() { return minYear; }

    public static int getMaxYear() { return maxYear; }

    public static int getNbYears() { return maxYear - minYear + 1; }

    public static void setMinYear(int minYear) { Context.minYear = minYear; }

    public static void setMaxYear(int maxYear) { Context.maxYear = maxYear; }

    public static boolean isConnectedR() { return connectedR; }

    public static void setConnectedR(boolean connectedR) { Context.connectedR = connectedR; }

    public static boolean isBuiltR() { return builtR; }

    public static void setBuiltR(boolean builtR) { Context.builtR = builtR; }

    public static boolean isSampledR() { return sampledR; }

    public static void setSampledR(boolean sampledR) { Context.sampledR = sampledR; }

    public static boolean isStarted() { return started; }

    public static void setStarted(boolean started) { Context.started = started; }

    public static double getWidth() { return width; }

    public static void setWidth(double w) { width = w; }

    public static double getHeight() { return height; }

    public static void setHeight(double h) { height = h; }

    public static double getInsideMin() { return insideMin; }

    public static double getInsideMax() { return insideMax; }

    public static double getRadiusComponent() { return radiusComponent; }

    public static double getRadiusInternalLink() { return radiusInternalLink; }

    // ----------------------------------------------------------------------------------
    public static List<String> getObservationsYears() {
        List<String> liste = new ArrayList<>();
        for (int y = getMinYear(); y <= getMaxYear(); y++) {
            liste.add(Integer.toString(y));
        }
        return (liste);
    }

    public static void init() {
        minYear = 4000;
        maxYear = -1000;
        workingDirectory = new File(System.getProperty("user.dir"));
        started = false;
        System.out.println("init");
        initRCaN();
        RCanInstalled = false;
        Rectangle2D rect = Screen.getPrimary().getVisualBounds();
        double screenWidth = rect.getWidth();
        double screenHeight = rect.getHeight();
        setWidth(0.9 * screenWidth);
        setHeight(0.9 * screenHeight);
        changed = false;
    }

    public static void initRCaN() {
        // System.out.println("init rcan");
        connectedR = false;
        builtR = false;
        sampledR = false;
    }
}
