package fr.cm.canObjects;

import fr.cm.GUIdialogs.AddObservationDialog;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.excel.ExcelManagerCSV;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class DataFile {
    private final StringProperty shortName = new SimpleStringProperty(this, "name");
    private final StringProperty fullFileName = new SimpleStringProperty(this, "fullFileName");
    String owner;
    boolean stillExisting;
    String metaInformationAboutDataFile;
    List<Observation> columnsInThisFilesAddedAsObservations;
    private List<String> namesColumnInFile;
    String[][] dataInFile;

    int firstYear, lastYear;

    // --------------------------------------------
    public DataFile(String shortName,
                    String fullFileName,
                    String metaInformationAboutDataFile,
                    String owner,
                    String codedObservations) {
        this.owner = owner;
        this.metaInformationAboutDataFile = metaInformationAboutDataFile;
        setShortName(shortName);
        setFullFileName(fullFileName);
        columnsInThisFilesAddedAsObservations = new ArrayList<>();
        decodeAddedObservations(codedObservations);
        readData();
    }

    // --------------------------------------------
    public DataFile(String fullFileName) {
        try {
            setFullFileName(fullFileName);
            File file = new File(fullFileName);
            setShortName(file.getName());
            owner = "Owner";
            metaInformationAboutDataFile = "Information about dataFile";
            columnsInThisFilesAddedAsObservations = new ArrayList<>();
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
        stillExisting = false;
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
                stillExisting = true;
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
    public Observation observationFromColumn(String nameColumn) {
        int numObs = namesColumnInFile.indexOf(nameColumn);
        int ny = dataInFile.length - 1;
        double[] values = new double[ny];
        for (int y = 0; y < ny; y++) {
            values[y] = correct(dataInFile[y][numObs]);
        }
        Observation observation = new Observation(this, nameColumn, values, firstYear, lastYear);
        return observation;
    }

    public void addObservationFromColumn(Observation observation) {
        columnsInThisFilesAddedAsObservations.add(observation);
    }

    public void removeObservationFromColumn(Observation observation) {
        columnsInThisFilesAddedAsObservations.remove(observation);
    }

    public Observation newObservationFromColumn(String nameColumn) {
        if (!ProjectListsManager.containsObservation(nameColumn)) {
            Observation observation =  observationFromColumn(nameColumn);
            new AddObservationDialog(observation, this);
            return(observation);
        }
        else {
            HelpDialog.warning("An observation with name " + nameColumn + " already exists", "Warning");
            return(null);
        }
    }

    // --------------------------------------------
    public String codeAddedObservations() {
        StringBuilder bo = new StringBuilder("");
        for (Observation observation : columnsInThisFilesAddedAsObservations) {
            bo.append(observation.codeInDataFile());
            bo.append(";");
        }
        return bo.toString();
    }

    // --------------------------------------------
    public void decodeAddedObservations(String codeObservations) {
        columnsInThisFilesAddedAsObservations = new ArrayList<>();
        String[] stObservations = codeObservations.split(";");
        for (String stObservation : stObservations) {
            if (!stObservation.equals("")) {
                Observation observation = Observation.codeFromDataFile(stObservation);
                observation.setOriginalFile(this);
                columnsInThisFilesAddedAsObservations.add(observation);
            }
        }
    }

    // --------------------------------------------
    public void updateObservations(List<Observation> listOfAddedObservations, List<Observation> listORemovedObservations) {
        for (Observation observation : listOfAddedObservations) {
            addObservationFromColumn(observation);
        }
        for (Observation observation : listORemovedObservations) {
            removeObservationFromColumn(observation);
        }
        columnsInThisFilesAddedAsObservations = columnsInThisFilesAddedAsObservations.stream().distinct().collect(Collectors.toList());
    }

    // --------------------------------------------
    public String getShortName() {
        return shortName.get();
    }

    public StringProperty getShortNameProperty() {
        return shortName;
    }

    public StringProperty getFullFileNameProperty() {
        return fullFileName;
    }

    public String getFullFileName() {
        return fullFileName.get();
    }

    public String getMetaInformationAboutDataFile() {
        return metaInformationAboutDataFile;
    }

    public void setMetaInformationAboutDatatFile(String metaInformationAboutDataFile) {
        this.metaInformationAboutDataFile = metaInformationAboutDataFile;
    }

    public void setFullFileName(String fullFileName) {
        this.fullFileName.set(fullFileName);
    }

    public void setShortName(String shortName) {
        this.shortName.set(shortName);
    }

    public List<String> getNamesColumnInFile() {
        return namesColumnInFile;
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

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    public boolean isStillExisting() {
        return stillExisting;
    }

}
