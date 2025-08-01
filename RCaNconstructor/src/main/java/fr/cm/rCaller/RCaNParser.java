package fr.cm.rCaller;

import com.github.rcaller.rstuff.RCaller;
import com.github.rcaller.rstuff.ROutputParser;
import fr.cm.xmlFiles.RCommandXML;

import java.util.ArrayList;
import java.util.List;

public class RCaNParser {

    public static String decodeParser(RCaller caller, RCommandXML rCommandXML) {
        ROutputParser parser = caller.getParser();
        String result;
        if (rCommandXML.isTable()) {
            result =  decodeArray(parser, rCommandXML);
        } else {
            result = decodeString(parser, rCommandXML);
        }
        return(result);
    }
    // ------------------------------------------------------------------------
    static String decodeArray(ROutputParser parser, RCommandXML rCommandXML) {
        List<ArrayList<String>> listOfLists = new ArrayList<ArrayList<String>>();
        List<String> na = parser.getNames();
        for (String n : na) {
            ArrayList<String> list = new ArrayList<String>();
            list.add(n);
            String[] cnt = parser.getAsStringArray(n);
            for(String cn:cnt) list.add(cn);
            listOfLists.add(list);
        }
        StringBuilder sb = new StringBuilder("");
        sb.append(rCommandXML.getTextMenu());
        sb.append("\n\n");
        int nbv = listOfLists.size();
        int nbi = listOfLists.get(0).size();

        int[] sz = new int[nbv];
        for(int v = 0; v < nbv; v++) {
            sz[v] = 10;
            for (int i = 0; i < nbi; i++) {
                if (i < listOfLists.get(v).size()) {
                    sz[v] = Math.max(sz[v], 3 + listOfLists.get(v).get(i).length());
                }
            }
        }
        for(int i=0;i<nbi;i++){
            for(int v = 0; v < nbv; v++){
                if(i< listOfLists.get(v).size())     {
                    sb.append(fmt(listOfLists.get(v).get(i),sz[v]));
                }
            }
            sb.append("\n");
        }
        return (sb.toString());
    }
    // ------------------------------------------------------------------------
    static String decodeString(ROutputParser parser, RCommandXML rCommandXML) {
        StringBuilder sb = new StringBuilder("");
        List<String> na = parser.getNames();
        for (String n : na) {
            sb.append("\n\n");
            String[] cnt = parser.getAsStringArray(n);
            for(String cn:cnt) {
                sb.append(cn);
            }
        }
        return (sb.toString());
    }
    // ------------------------------------------------------------------------
    static String fmt(String st, int s) {
        String b = "                                                       ";
        String fm;
        try {
            double d = Double.parseDouble(st);
            fm = String.format("%.3f", d);
        } catch (NumberFormatException e) {
            fm = st;
        }
        int l = fm.length();
        String rs;
        try{
            rs = b.substring(0, s + 4 - l) + fm;
        }
        catch (Exception e){
            rs = fm;
        }
        return rs;
    }


}
