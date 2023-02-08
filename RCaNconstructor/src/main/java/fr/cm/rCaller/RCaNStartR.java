package fr.cm.rCaller;

import com.github.rcaller.graphics.SkyTheme;
import com.github.rcaller.rstuff.*;
import fr.cm.Main.Context;

public class RCaNStartR extends Thread{
    static RCaller caller;
    static RCode code;
    static boolean connected;
    static StringBuilder succ = new StringBuilder("History of connection \n");

    public void run(){
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
    // -------------------------------------------------------
    public static RCode getCode() {

        return code;
    }
    public static RCaller getCaller() {

        return caller;
    }

    public static String getSucc() {

        return succ.toString();
    }

    public static void succAppend(String s){
        succ.append(s);
    }
}
