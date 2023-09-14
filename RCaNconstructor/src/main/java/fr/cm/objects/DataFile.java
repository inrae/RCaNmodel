package fr.cm.objects;

import fr.cm.dialogs.HelpDialog;
import fr.cm.Main.ObjectsManager;
import fr.cm.excel.ExcelManagerCSV;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public class DataFile {

    private String id, owner, metaInformation;
    private final StringProperty shortName = new SimpleStringProperty(this, "name"),
            fullFileName = new SimpleStringProperty(this, "fullFileName");
    boolean correctlyRead;
    List<Observation> addedObservations;
    private List<String> namesColumnInFile;
    String[][] dataInFile;
    int firstYear, lastYear;
    // --------------------------------------------
    public DataFile(String id,
                    String shortName,
                    String fullFileName,
                    String metaInformationAboutDataFile,
                    String owner,
                    List<Observation> addedObservations) {
        this.id = id;
        this.owner = owner;
        this.metaInformation = metaInformationAboutDataFile;
        setShortName(shortName);
        setFullFileName(fullFileName);
        this.addedObservations = addedObservations;
        for (Observation observation : addedObservations) {
            observation.setOriginalFile(this);
            }
        readData();
    }

    // --------------------------------------------
    public DataFile(String fullFileName) {
        try {
            id = ObjectsManager.getDataFileId();
            setFullFileName(fullFileName);
            File file = new File(fullFileName);
            setShortName(file.getName());
            owner = "Owner";
            metaInformation = "Information about dataFile";
            addedObservations = new ArrayList<>();
            readData();
        }
        catch(Exception ex){}
    }

    // --------------------------------------------
    public double correct(String str) {
        double parseDouble = Double.parseDouble(str.replace(",", "."));
        return parseDouble;
    }

    // --------------------------------------------
    void readData() {
        namesColumnInFile = new ArrayList<>();
        correctlyRead = false;
        String [][] valuesCSV = ExcelManagerCSV.importCSV(fullFileName.get());
        if (valuesCSV != null) {
            int nl, nc;
            try {
                nl = valuesCSV.length - 1;
                nc = valuesCSV[0].length;
                dataInFile = new String[nl - 1][nc];
                for (int c = 0; c < nc; c++) {
                    namesColumnInFile.add(valuesCSV[0][c]);
                    for (int l = 0; l < nl - 1; l++) {
                        dataInFile[l][c] = valuesCSV[l + 1][c];
                    }
                }
                firstYear = Integer.parseInt(dataInFile[1][0]);
                lastYear = Integer.parseInt(dataInFile[nl - 2][0]);
                correctlyRead = true;
            }
            catch (Exception ex) {
                HelpDialog.warning("Problem in reading file " + fullFileName.get(), "Warning", ex);
            }
        }
    }

    public int getNumberOfColumns(){

        return(namesColumnInFile.size());
    }
    // --------------------------------------------

    public void removeAddedObservation(Observation observation) {

        addedObservations.remove(observation);
        ObjectsManager.removeObservation(observation);
    }

    public Observation makeObservationFromColumn(String nameColumn) {
        String nameObs = id + "_" + nameColumn;
        if ( ! ObjectsManager.containsObservation(nameObs)) {
            int numObs = namesColumnInFile.indexOf(nameColumn);
            int ny = dataInFile.length - 1;
            double[] values = new double[ny];
            for (int y = 0; y < ny; y++) {
                values[y] = correct(dataInFile[y][numObs]);
            }
            Observation observation = new Observation(this, nameObs, values, firstYear, lastYear);
            ObjectsManager.addObservation(observation,true);
            addedObservations.add(observation);
            return(observation);
        }
        else {
            HelpDialog.warning("An observation with name " + nameColumn + " already exists", "Warning");
            return(null);
        }
    }

    String form(String s){
        if(s.contains(".")){
            int pp = s.indexOf(".");
            int ll = s.length();
            int mm = Math.min(ll,pp+2);
            return(s.substring(0,mm));
        }
        return(s);
    }
    public String[][] getFormattedDataInFile() {
        int nl = dataInFile.length;
        int nc = dataInFile[0].length;
        String[][] formatedData = new String[nl][nc];
        for(int l = 0; l <nl; l++)
            for(int c=0; c < nc; c++)
                formatedData[l][c] = form(dataInFile[l][c]);
        return formatedData;
    }
    // --------------------------------------------------
    public String getStringAddedObservations() {
        StringBuilder bo = new StringBuilder("");
        for (Observation observation : addedObservations) {
            bo.append(observation.getObsName());
            bo.append("\n");
        }
        return bo.toString();
    }

    public String getId() {
        return id;
    }
    public String getShortName() {
        return shortName.get();
    }

    public void setShortName(String shortName) {
        this.shortName.set(shortName);
    }

    public String getFullFileName() {
        return fullFileName.get();
    }

    public StringProperty fullFileNameProperty() {
        return fullFileName;
    }

    public void setFullFileName(String fullFileName) {
        this.fullFileName.set(fullFileName);
    }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public boolean isCorrectlyRead() {
        return correctlyRead;
    }

    public String getMetaInformation() {
        return metaInformation;
    }

    public void setMetaInformation(String metaInformation) {
        this.metaInformation = metaInformation;
    }

    public List<Observation> getAddedAsObservations() {
        return addedObservations;
    }

    public List<String> getNamesColumnInFile() {
        return namesColumnInFile;
    }

    public int getFirstYear() {
        return firstYear;
    }

    public int getLastYear() {
        return lastYear;
    }

}
