/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import fr.cm.menus.Context;

/**
 * @author christianmullon
 */
public class Observation {

    private double[] values;
    String obsName;
    String originalFile;
    String originalColumn;
    String unit;

    public Observation(String obsName, double[] tValues) {
        this.obsName = obsName;
        this.values = tValues;
        originalFile = "-";
        originalColumn = "-";
        unit = "MT";
      }

    public Observation(String obsName, double [] nValues, boolean y){
        this.obsName = obsName;
        int nv = nValues.length;
        values = new double[nv];
        originalFile = "-";
        originalColumn = "-";
        unit = "MT";
        System.arraycopy(nValues,0,values,0,nv);
     }

    public static Observation codeFromExternalFile(String codeObservation){
        String[] codes = codeObservation.split(",");
        if(codes.length>1){
            String nObsName = codes[0];
            Observation observation = ListsManager.getObservationByName(nObsName);
            observation.setOriginalFile(codes[1]);
            observation.setOriginalColumn(codes[2]);
            observation.setUnit(codes[3]);
            return(observation);
        } else {
            return(null);
        }
     }

    public String codeInExternalFile(){
         StringBuilder sb = new StringBuilder("");
         Observation observation = ListsManager.getObservationByName(obsName);
         if(observation == null) return(sb.toString());
         else {
             sb.append(obsName);
             sb.append(",");
             sb.append(originalFile);
             sb.append(",");
             sb.append(originalColumn);
             sb.append(",");
             sb.append(unit);
         }
         return(sb.toString());
     }

    public Observation(String fileName, String colName, double[] tValues, int ofirst, int olast) {
        this.originalFile = fileName;
        this.originalColumn = colName;
        this.unit = "MT";
        this.obsName = originalFile + "." + colName;
        int afirst = ofirst;
        int alast = olast;
        if (ListsManager.getListOfObservations().size() > 0) {
            afirst = Context.getMinYear();
            alast = Context.getMaxYear();
        }
        int first = Math.min(ofirst, afirst);
        int last = Math.max(olast, alast);
        int ny = last - first + 1;
        Context.setMinYear(first);
        Context.setMaxYear(last);
        if (first == ofirst && last == olast) {
            this.setValues(tValues);
        } else {
            double[] newValues = new double[ny];
            for (int y = first; y <= last; y++) {
                newValues[y - first] = -1;
                if ((y >= ofirst) && (y <= olast)) {
                    newValues[y - first] = tValues[y - ofirst];
                }
            }
            this.setValues(newValues);
        }
        if(! ListsManager.containsObservation(obsName)) {
            if ((first != afirst) || (last != alast)) {
                double[] newValues = new double[ny];
                for (int o = 0; o < ListsManager.getListOfObservations().size(); o++) {
                    Observation observation = ListsManager.getListOfObservations().get(o);
                    double[] oldValues = observation.getValues();
                    for (int y = first; y <= last; y++) {
                        newValues[y - first] = -1;
                        if ((y >= afirst) && (y <= alast)) {
                            newValues[y - first] = oldValues[y - afirst];
                        }
                    }
                    observation.setValues(newValues);
                }
            }
        }
    }

    public void setOriginalFile(String originalFile) { this.originalFile = originalFile; }

    public String getObsName() { return obsName; }

    public void setObsName(String obsName) { this.obsName = obsName; }

    public void setOriginalColumn(String originalColumn) { this.originalColumn = originalColumn; }

    public String getUnit() { return unit; }

    public void setUnit(String unit) { this.unit = unit; }

    public double[] getValues() { return values; }

    public double getValues(int y) { return (values[y]); }

    public void setValues(double[] newValues) {
        int nv = newValues.length;
        values = new double[nv];
        System.arraycopy(newValues, 0, values, 0, nv);
    }

    public String getMeta(){
        StringBuilder sb = new StringBuilder("");
        sb.append("Observation name : " + obsName + "\n");
        sb.append("From file :  " + originalFile + "\n");
        sb.append("Column :  " + originalColumn + "\n");
        return(sb.toString());
    }
}

