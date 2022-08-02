package fr.cm.xmlFiles;

import fr.cm.Main.Context;
import fr.cm.rCaller.RCaNCommon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RCommandXML {

    private String name;
    private String textMenu;
    private String menu;
    private List<String> rCompute;
    private List<String> rPlots;
    private String help;
    private String condition;
    private String typeParameter;
    private String table;

    static String parameter ="";

    // ------------------------------------------------------------------------
    public RCommandXML(String name,
                       String textMenu,
                       String menu,
                       String rCompute,
                       String rPlots,
                       String help,
                       String condition,
                       String typeParameter,
                       String table) {
        this.name = nolf(name);
        this.textMenu = nolf(textMenu);
        this.menu = nolf(menu);
        this.help = help;
        this.table = nolf(table);
        this.condition = nolf(condition);
        this.typeParameter = nolf(typeParameter);
        this.rCompute = new ArrayList<>();
        List<String> sCommands = Arrays.asList(rCompute.split("\n"));
        for(String st : sCommands) {
            String tst = nolf(st);
            if(tst.length()>0) {
                this.rCompute.add(tst);
            }
        }
        this.rPlots = new ArrayList<>();
        List<String> sPlots = Arrays.asList(rPlots.split("\n"));
        for(String st : sPlots) {
            String tst = nolf(st);
            if(tst.length()>0) {
                this.rPlots.add(tst);
            }
        }
    }
    // ------------------------------------------------------------------------
    public String nolf(String st){
        return(st.replace("\n","").trim());
    }
    // ------------------------------------------------------------------------
    public void setState(boolean ok){
        switch(name){
            case "connect":
                Context.setConnectedR(ok);
                break;
            case "build":
                Context.setBuiltR(ok);
                break;
            case "sample":
                Context.setSampledR(ok);
                break;
        }
    }
    // ------------------------------------------------------------------------
    public boolean conditionOK(){
        boolean ok = true;
        switch(condition){
            case "connected":
                return(Context.isConnectedR());
            case "built":
                return(Context.isBuiltR());
            case "sampled":
                return(Context.isSampledR());
        }
        return(ok);
    }
    // ------------------------------------------------------------------------
    public String getStringCommandLine() {
        StringBuilder sb = new StringBuilder("R commands \n");
        List<String> rC = this.getrCompute();
        List<String> rP = this.getrPlots();
        sb.append("------\n");
        for (String st : rC) {
            sb.append(explicitCommandLine(st));
            sb.append("\n ");
        }
        sb.append("------\n");
        for (String st : rP) {
            sb.append(explicitCommandLine(st));
            sb.append("\n");
        }
        sb.append("------\n");
        String st = sb.toString();
        return (st);
    }
    // ------------------------------------------------------------------------
    public String getShortStringCommandLine() {
        StringBuilder sb = new StringBuilder("R command \n");
        List<String> rC = this.getrCompute();
        List<String> rP = this.getrPlots();
        if(rC.size()>0){
            sb.append(explicitCommandLine(rC.get(0)));
            sb.append("\n ");
        }
        if(rP.size()>0){
            sb.append(explicitCommandLine(rP.get(0)));
            sb.append("\n");
        }
        String st = sb.toString();
        return (st);
    }
    // ------------------------------------------------------------------------
    public String explicitCommandLine(String commandLine) {
        String sCommandLine = commandLine
                .replace("&lt;", "<")
                .replace("FILENAME", Context.getFullFileName())
                .replace("SIZE", RCaNCommon.getParameterSizeSample())
                .replace("NCHAIN", RCaNCommon.getParameterNChain())
                .replace("THIN", RCaNCommon.getParameterThin());
        if(typeParameter.length()>0){
            sCommandLine = sCommandLine.replace(typeParameter, parameter);
        }
        return (sCommandLine);
    }

    public String actionCommandLine() {
        StringBuilder sCommandLine = new StringBuilder("R : ");
        for(String rCommand : rCompute){
            sCommandLine.append(explicitCommandLine(rCommand));
            sCommandLine.append(" ; ");
        }
        return (sCommandLine.toString());
    }
    // ------------------------------------------------------------------------
    public static String getParameter() {
        return parameter;
    }
    public static void setParameter(String parameter) {
        RCommandXML.parameter = parameter;
    }
    public String getTextMenu() { return textMenu; }

    public String getHelp() { return help; }

    public String getMenu() { return menu; }

    public List<String> getrCompute() {return rCompute; }

    public List<String> getrPlots() { return rPlots; }

    public String getName() { return name; }

    public String getTypeParameter() { return typeParameter; }

    public boolean isTable() { return table.equals("yes");  }

    public boolean isPlot() { return (rPlots.size()>0); };

    public boolean isDisconnect(){ return(name.equals("disconnect")); };

}
