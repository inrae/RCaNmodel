package fr.cm.Main;

import java.io.IOException;
import java.util.logging.FileHandler;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;

public class Logg {
    static Logger logger = Logger.getLogger("MyLog");
    static void createLog(){
        FileHandler fileHandler;
        try {
            // This block configure the logger with handler and formatter
            fileHandler = new FileHandler("rCaNlog.txt");
            logger.addHandler(fileHandler);
            SimpleFormatter formatter = new SimpleFormatter();
            fileHandler.setFormatter(formatter);
        } catch (SecurityException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
    public static void addLog(String msg){
        try {
            logger.info(msg);
        } catch (SecurityException e) {
            e.printStackTrace();
        }
    }

}
