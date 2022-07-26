package fr.cm.xmlFiles;

import fr.cm.RCaNMain.Context;
import fr.cm.rCaller.RCaNCommon;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RCommandXML {

    private String name;
    private String textMenu;
    private String subMenu;
    private List<String> rCompute;
    private List<String> rPlots;
    private String help;
    private String condition;
    private String typeParameter;
    private String table;

    static String parameter ="";
    public static String getParameter() {
        return parameter;
    }
    public static void setParameter(String parameter) {
        RCommandXML.parameter = parameter;
    }
    
    // ------------------------------------------------------------------------
    public RCommandXML(String name,
                       String textMenu,
                       String subMenu,
                       String rCompute,
                       String rPlots,
                       String help,
                       String condition,
                       String typeParameter,
                       String table) {
        this.name = nolf(name);
        this.textMenu = nolf(textMenu);
        this.subMenu = nolf(subMenu);
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
    public String getSt() {
        StringBuilder sb = new StringBuilder("R commands \n");
        List<String> rC = this.getrCompute();
        List<String> rP = this.getrPlots();
        sb.append("------\n");
        for (String st : rC) {
            sb.append(completeCommandLine(st));
            sb.append("\n ");
        }
        sb.append("------\n");
        for (String st : rP) {
            sb.append(completeCommandLine(st));
            sb.append("\n");
        }
        sb.append("------\n");
        String st = sb.toString();
        return (st);
    }
    // ------------------------------------------------------------------------
    public String completeCommandLine(String commandLine) {
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
            sCommandLine.append(completeCommandLine(rCommand));
            sCommandLine.append(" ; ");
        }
        return (sCommandLine.toString());
    }
    // ------------------------------------------------------------------------
    public String getTextMenu() { return textMenu; }

    public String getHelp() { return help; }

    public String getSubMenu() { return subMenu; }

    public List<String> getrCompute() {return rCompute; }

    public List<String> getrPlots() { return rPlots; }

    public String getName() { return name; }

    public String getTypeParameter() { return typeParameter; }

    public boolean isTable() { return table.equals("yes");  }

    public boolean isPlot() { return (rPlots.size()>0); };

    public boolean isDisconnect(){ return(name.equals("disconnect")); };

}
