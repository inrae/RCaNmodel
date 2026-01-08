/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.objects;

import fr.cm.Main.ObjectsManager;
import fr.cm.Main.Context;

/**
 * @author christianmullon
 */
public class Observation {
    private double[] values;
    DataFile dataFile;
    String obsName, originalColumn, unit;
    static int firstYear, lastYear;
    // --------------------------------------------
    public Observation(String obsName, double [] nValues){
        this.obsName = obsName;
        int nv = nValues.length;
        values = new double[nv];
        dataFile = null;
        originalColumn = "-";
        unit = "MT";
        System.arraycopy(nValues,0,values,0,nv);
     }
    // --------------------------------------------
    public Observation(DataFile dataFile, String colName, double[] tValues, int ofirst, int olast) {
        this.dataFile = dataFile;
        this.originalColumn = colName;
        this.unit = "MT";
        this.obsName = colName;
        int afirst = ofirst;
        int alast = olast;
        if (ObjectsManager.getListOfObservations().size() > 0) {
            afirst = Context.getFirstYear();
            alast = Context.getLastYear();
        }
        int first = Math.min(ofirst, afirst);
        int last = Math.max(olast, alast);
        int ny = last - first + 1;
        Context.setFirstYear(first);
        Context.setLastYear(last);
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
        if(! ObjectsManager.containsObservation(obsName)) {
            if ((first != afirst) || (last != alast)) {
                double[] newValues = new double[ny];
                for (int o = 0; o < ObjectsManager.getListOfObservations().size(); o++) {
                    Observation observation = ObjectsManager.getListOfObservations().get(o);
                    double[] oldValues = observation.getValues();
                    for (int y = first; y <= last; y++) {
                        newValues[y - first] = -1;
                        if ((y >= afirst) && (y <= alast)) {
                            newValues[y - first] = oldValues[y - afirst];
                        }
                    }
                    observation.setValues(newValues);
                    observation.setFirstYear(first);
                    observation.setLastYear(last);
                }
            }
        }
    }
    // --------------------------------------------
    public static Observation codeFromDataFile(String codeObservation){
        String[] codes = codeObservation.split(",");
        if(codes.length>1) {
            String nObsName = codes[0];
            Observation observation = ObjectsManager.getObservationByName(nObsName);
            observation.setOriginalFile(ObjectsManager.getDataFileByName(codes[1]));
            observation.setOriginalColumn(codes[2]);
            observation.setUnit(codes[3]);
            if (codes.length > 5) {
                try {
                    observation.setFirstYear(Integer.getInteger(codes[4]));
                    observation.setLastYear(Integer.getInteger(codes[5]));
                }
                catch(Exception ex){
                    observation.setFirstYear(Context.getFirstYear());
                    observation.setLastYear(Context.getLastYear());
                }
            } else {
            observation.setFirstYear(Context.getFirstYear());
            observation.setLastYear(Context.getLastYear());
            }
            return(observation);
        } else {
            return(null);
        }
    }
    // --------------------------------------------
    public String codeInDataFile(){
        StringBuilder sb = new StringBuilder("");
        Observation observation = ObjectsManager.getObservationByName(obsName);
        if(observation == null) return(sb.toString());
        else {
            sb.append(obsName);
            sb.append(",");

            sb.append(dataFile.getShortName());
            sb.append(",");
            sb.append(originalColumn);
            sb.append(",");
            sb.append(unit);
            sb.append(",");
            sb.append(String.valueOf(firstYear));
            sb.append(",");
            sb.append(String.valueOf(lastYear));
        }
        return(sb.toString());
    }
    public void setOriginalFile(DataFile dataFile) { this.dataFile = dataFile; }
    public String getObsName() { return obsName; }
    public void setObsName(String obsName) { this.obsName = obsName; }
    public void setOriginalColumn(String originalColumn) { this.originalColumn = originalColumn; }
    public void setUnit(String unit) { this.unit = unit; }
    public double[] getValues() { return values; }
    public double getValues(int y) { return (values[y]); }
    public void setValues(double[] newValues) {
        int nv = newValues.length;
        values = new double[nv];
        System.arraycopy(newValues, 0, values, 0, nv);
    }
    public void setFirstYear(int firstYear) {
        this.firstYear = firstYear;
    }
    public void setLastYear(int lastYear) {
        this.lastYear = lastYear;
    }
    // --------------------------------------------
    public String getMeta(){
        StringBuilder sb = new StringBuilder("");
        sb.append("Observation name : " + obsName + "\n");
        sb.append("From file :  " + dataFile.getShortName() + "\n");
        sb.append("Column :  " + originalColumn + "\n");
        return(sb.toString());
    }
    // --------------------------------------------

}

