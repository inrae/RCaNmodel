package fr.cm.rCaller;

import fr.cm.Main.Context;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class RCaNScript {

    private String name, textMenu, menu, help, condition, typeParameter, table;
    private List<String> rCompute, rPlots;

    // ------------------------------------------------------------------------
    public RCaNScript(String name,
                      String textMenu,
                      String menu,
                      String rCompute,
                      String rPlots,
                      String help,
                      String condition,
                      String typeParameter,
                      String table) {
        this.name = noLinefeed(name);
        this.textMenu = noLinefeed(textMenu);
        this.menu = noLinefeed(menu);
        this.help = help;
        this.table = noLinefeed(table);
        this.condition = noLinefeed(condition);
        this.typeParameter = noLinefeed(typeParameter);
        this.rCompute = new ArrayList<>();
        List<String> sCommands = Arrays.asList(rCompute.split("\n"));
        for(String st : sCommands) {
            String tst = noLinefeed(st);
            if(tst.length()>0) {
                this.rCompute.add(tst);
            }
        }
        this.rPlots = new ArrayList<>();
        List<String> sPlots = Arrays.asList(rPlots.split("\n"));
        for(String st : sPlots) {
            String tst = noLinefeed(st);
            if(tst.length()>0) {
                this.rPlots.add(tst);
            }
        }
    }
    // ------------------------------------------------------------------------
    public void setScriptParameters() {
        if (getName().equals("sample")) {
            new RCaNDialogGetParametersForSampling(this);
        } else {
            new RCaNDialogGetParametersForRCommand(this);
        }

    }

   // ------------------------------------------------------------------------
    public void setSamplingParameters(String size, String nchain, String thin) {
        for (int c=0; c<rCompute.size(); c++) {
            String st = rCompute.get(c);
            st = st.replace("&lt;", "<")
                    .replace("FILENAME", Context.getFullFileName())
                    .replace("SIZE", size)
                    .replace("NCHAIN", nchain)
                    .replace("THIN", thin);
            rCompute.set(c,st);
        }
      }
    // ------------------------------------------------------------------------
    public void setCommandParameters(String parameter) {
        for (int c=0; c<rCompute.size(); c++) {
            String st = rCompute.get(c)
                    .replace("&lt;", "<")
                    .replace("FILENAME", Context.getFullFileName())
                    .replace(typeParameter, parameter);
            rCompute.set(c,st);
        }
        for (int c=0; c<rPlots.size(); c++) {
            String st = rPlots.get(c)
                    .replace("&lt;", "<")
                    .replace("FILENAME", Context.getFullFileName())
                    .replace(typeParameter, parameter);
            rPlots.set(c,st);
        }
    }


    // ------------------------------------------------------------------------
    public String getScriptString() {
        StringBuilder sb = new StringBuilder("R commands \n");
        sb.append("------\n");
        for (String st : rCompute) sb.append(st).append("\n ");
        sb.append("------\n");
        for (String st : rPlots) sb.append(st).append("\n");
        sb.append("------\n");
        return (sb.toString());
    }    // ------------------------------------------------------------------------
    public void print() {
        System.out.println(getScriptString());
    }
    // ------------------------------------------------------------------------
    public String getShortScript() {
        StringBuilder sb = new StringBuilder("R commands \n");
        if(rCompute.size()>0) sb.append(rCompute.get(0));
        if(rPlots.size()>0) sb.append(rPlots.get(0));
        return (sb.toString());
    }    // ------------------------------------------------------------------------
    String noLinefeed(String st){
        return(st.replace("\n"," ").trim());
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

    public String getTextMenu() { return textMenu; }

    public String getHelp() { return help; }

    public String getMenu() { return menu; }

    public List<String> getrCompute() {return rCompute; }

    public List<String> getrPlots() { return rPlots; }

    public String getName() { return name; }

    public String getTypeParameter() { return typeParameter; }

    public boolean isTable() { return table.equals("yes");  }

    public boolean isPlot() { return (rPlots.size()>0); };

}
