package fr.cm.ProjectManager;

import fr.cm.GUInetwork.NetworkView;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.canObjects.*;
import fr.cm.excel.ExcelManager;
import fr.cm.parameters.Strings;
import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ProjectListsManager {

    private static List<Component> listOfComponents;
    private static List<Flux> listOfFluxes;
    private static List<Constraint> listOfConstraints;
    private static List<Observation> listOfObservations;
    private static List<DataFile> listOfDataFiles;
    private static List<Action> listOfActions;
    private static List<MetaElement> listOfMetaElements;

    private static NetworkView networkView;

    // --------------------------------------------
    public static void init() {
        networkView = new NetworkView();
        networkView.init();
        listOfComponents = new ArrayList<>();
        listOfFluxes = new ArrayList<>();
        listOfConstraints = new ArrayList<>();
        listOfObservations = new ArrayList<>();
        listOfDataFiles = new ArrayList<>();
        listOfActions = new ArrayList<>();
        listOfMetaElements =  new ArrayList<>();
    }
    // ------------------------------------------------------------------------------
    // ACTIONS

    public static void addAction(Action action, boolean save) {

        listOfActions.add(action);
        if(save) saveExcel();

    }
    public static void addAction(String comment, boolean save) {
        Action action = new Action(comment);
        listOfActions.add(action);
        if(save) saveExcel();
    }

    public static List<Action> getListOfActions() {

        return listOfActions;
    }

    public static void initListOfActions() {

        listOfActions = new ArrayList<>();
    }

    public static void setListOfActions(List<Action> listOfActions) {
        ProjectListsManager.listOfActions = listOfActions;
    }

    public static void updateAction(int nu, String comment) {
        Action action = listOfActions.get(nu);
        action.setCommentAuthor(comment);
        saveExcel();
    }

    // ------------------------------------------------------------------------------
    // META INFORMATION
    public static void makeMetaElementsList(){
        listOfMetaElements = new ArrayList<>();
        String fileName = "project/Project.txt";
        InputStream inst = ProjectListsManager.class.getClassLoader().getResourceAsStream(fileName);
        if(inst != null){
            List<String> elementsResource = new ArrayList<>();
            try {
                elementsResource = IOUtils.readLines(inst, StandardCharsets.UTF_8.name());
            } catch (IOException e) {
                e.printStackTrace();
            }
            for(String elementR : elementsResource){
                MetaElement metaElement = new MetaElement(elementR);
                listOfMetaElements.add(metaElement);
            }
        }
    }

    public static List<MetaElement> getListOfMetaElements() {
        return listOfMetaElements;
    }

    public static void setListOfMetaElements(List<MetaElement> listOfMetaElements) {
        ProjectListsManager.listOfMetaElements = listOfMetaElements;
    }

    public static void updateMetaElement(int nu, String comment) {
        listOfMetaElements.get(nu).setMetaContent(comment);
        saveExcel();
    }

    public static boolean metaInformationType(int nu){
        return listOfMetaElements.get(nu).isMetaType();
    }
    // ------------------------------------------------------------------------------
    // NETWORK VIEW
    public static NetworkView getNetworkView() {
        networkView.update();
        return networkView;
    }
    // ------------------------------------------------------------------------------
    // COMPONENTS
    public static void addComponent(Component component, boolean save) {
        if (containsComponent(component.getName())) {
            HelpDialog.warning("This component already exists","Warning");
        } else  if(containsObservation(component.getName())){
            HelpDialog.warning("An observation with this name already exists","Warning");
        } else{
            component.define(
                    networkView.getMousePressedEventHandler(),
                    networkView.getMouseDraggedEventHandler(),
                    networkView.getMouseDoubleClickOnCircleEventHandler());
            listOfComponents.add(component);
        }
        addAction("Added component: "+component.getName(),save);
    }
    public static void removeComponent(Component component) {
        listOfConstraints.removeIf(constraint -> constraint.involve(component));
        Iterator<Flux> iLink = listOfFluxes.iterator();
        while (iLink.hasNext()) {
            Flux flux = iLink.next();
            if (flux.hasExtremities(component)) {
                iLink.remove();
                listOfConstraints.removeIf(constraint -> constraint.involve(component));
            }
        }
        listOfComponents.remove(component);
        addAction("Removed component: "+component.getName(),true);
    }
    public static boolean containsComponent(String groupName) {
        boolean contain = false;
        for (Component component : listOfComponents){
            if (groupName.equals(component.getName())) {
                contain = true;
                break;
            }
        }
        return contain;
    }
    public static Component whichComponent(String group) {
        for (Component component : listOfComponents){
            if (group.equals(component.getName())) {
                return (component);
            }
        }
        return (null);
    }
    public static List<String> getNamesOfComponents() {
        List<String> groupNames = new ArrayList<>();
        for (Component component : listOfComponents){
            groupNames.add(component.getName());
        }
        return groupNames;
    }
    public static List<String> getNamesOfComponentsIn() {
        List<String> groupNames = new ArrayList<>();
        for (Component component : listOfComponents){
            if(component.isInside()) {
                groupNames.add(component.getName());
            }
        }
        return groupNames;
    }
    public static void upComponent(Component component) {
        int numComponent = listOfComponents.indexOf(component);
        if (numComponent > 0) {
            permuteListOfComponents(numComponent, numComponent - 1);
        }
    }
    public static void downComponent(Component component) {
        int numComponent = listOfComponents.indexOf(component);
        if (numComponent < listOfComponents.size() - 1) {
            permuteListOfComponents(numComponent, numComponent + 1);
        }
    }
    public static void permuteListOfComponents(int rowA, int rowB) {
        Component componentA = listOfComponents.get(rowA);
        Component componentB = listOfComponents.get(rowB);
        listOfComponents.set(rowA, componentB);
        listOfComponents.set(rowB, componentA);
    }
    public static List<Component> getListOfComponents() {
        return listOfComponents;
    }

    // ------------------------------------------------------------------------------
    // LINKS
    public static void addLink(Flux flux, boolean save) {
        if (containsLink(flux.getName())) {
            HelpDialog.warning("Link already exists","Warning");
        } else {
            flux.define(networkView.getMouseDoubleClickOnLineEventHandler());
            listOfFluxes.add(flux);
            addAction("Added Flux: "+ flux.getName(),save);
        }
    }
    public static List<String> getNamesOfLinks() {
        List<String> linksNames = new ArrayList<>();
        for (Flux flux : listOfFluxes){
            linksNames.add(flux.getName());
        }
        return linksNames;
    }
    public static boolean containsLink(String linkName) {
        boolean contain = false;
        for (Flux flux : listOfFluxes){
            if (linkName.equals(flux.getName())) {
                contain = true;
            }
        }
        return contain;
    }
    public static void removeLink(Flux flux) {
        listOfConstraints.removeIf(constraint -> constraint.involve(flux));
        listOfFluxes.remove(flux);
        addAction("Removed Flux: "+ flux.getName(),true);
    }
    public static void permuteListOfLinks(int rowA, int rowB) {
        Flux fluxA = listOfFluxes.get(rowA);
        Flux fluxB = listOfFluxes.get(rowB);
        listOfFluxes.set(rowA, fluxB);
        listOfFluxes.set(rowB, fluxA);
    }
    public static void upLink(Flux flux) {
        int numLink = listOfFluxes.indexOf(flux);
        if (numLink > 0) {
            permuteListOfLinks(numLink, numLink - 1);
        }
    }
    public static void downLink(Flux flux) {
        int numLink = listOfFluxes.indexOf(flux);
        if (numLink < listOfFluxes.size() - 1) {
            permuteListOfLinks(numLink, numLink + 1);
        }
    }
    public static List<Flux> getListOfFluxes() {
        return listOfFluxes;
    }

    // --------------------------------------------------------------------------
    //  OBSERVATIONS
    public static void addObservation(Observation observation, boolean save) {
        if(containsObservation(observation.getObsName())){
            HelpDialog.warning("Observation already exists","Warning");
        } else if(containsComponent(observation.getObsName())){
            HelpDialog.warning("A component with same name already exists","Warning");
        }
            else {
            listOfObservations.add(observation);
            addAction("Added Observation: "+ observation.getObsName(),save);
        }
    }
     public static void removeObservation(String observationName){
        for (Observation observation : listOfObservations)
            if (observationName.equals(observation.getObsName())) {
                listOfObservations.remove(observation);
                addAction("Removed Observation: "+ observation.getObsName(),true);
                break;
            }
    }
    public static void removeObservation(Observation observation){
        listOfObservations.remove(observation);
        addAction("Removed Observation: "+ observation.getObsName(),true);
    }

   public static boolean containsObservation(String observationName) {
        boolean contain = false;
        for (Observation observation : listOfObservations)
            if (observationName.equals(observation.getObsName())) {
                contain = true;
                break;
            }
        return contain;
    }
    public static Observation getObservationByName(String observationName) {
        Observation observationS = null;
        for (Observation observation : listOfObservations)
            if (observationName.equals(observation.getObsName())) {
                observationS = observation;
                break;
            }
        return observationS;
    }
    public static List<String> getNamesOfObservations() {
        List<String> observationNames = new ArrayList<>();
        for (Observation observation : listOfObservations) {
            observationNames.add(observation.getObsName());
        }
        return (observationNames);
    }
    public static List<Observation> getListOfObservations() {
        return listOfObservations;
    }

    public static void setListOfObservations(List<Observation> listOfObservations) {
        ProjectListsManager.listOfObservations = listOfObservations;
    }

    // ------------------------------------------------------------------------------
    // CONSTRAINTS
    public static void addConstraint(Constraint constraint, boolean save) {
        listOfConstraints.add(constraint);
        addAction("Added Constraint: "+ constraint.getName(),save);
    }
    public static void removeConstraints(Constraint constraint) {
        listOfConstraints.remove(constraint);
        addAction("Removed Constraint: "+ constraint.getName(),true);
    }
    public static void updateConstraint(Constraint oldC, Constraint newC) {
        int pos = listOfConstraints.indexOf(oldC);
        listOfConstraints.set(pos,newC);
        addAction("Updated Constraint: "+ oldC.getName()+ "-W" + newC.getName(),true);
    }
    public static void upConstraint(Constraint constraint) {
        int numConstraint = listOfConstraints.indexOf(constraint);
        if (numConstraint > 0) {
            permuteListOfConstraints(numConstraint, numConstraint - 1);
        }
    }
    public static void downConstraint(Constraint constraint) {
        int numConstraint = listOfConstraints.indexOf(constraint);
        if (numConstraint < listOfConstraints.size() - 1) {
            permuteListOfConstraints(numConstraint, numConstraint + 1);
         }
    }
    public static void permuteListOfConstraints(int rowA, int rowB) {
        Constraint groupA = listOfConstraints.get(rowA);
        Constraint groupB = listOfConstraints.get(rowB);
        listOfConstraints.set(rowA, groupB);
        listOfConstraints.set(rowB, groupA);
    }
    public static List<Constraint> getListOfConstraints() {
        return listOfConstraints;
    }

    // ------------------------------------------------------------------------------
    // DATA FILES
    public static void addDataFile(DataFile newDataFile, boolean save) {
        boolean in = false;
        for(DataFile dataFile : listOfDataFiles){
            if (dataFile.getShortName().equals(newDataFile.getShortName())) {
                in = true;
                break;
            }
        }
        if(! in) {
            listOfDataFiles.add(newDataFile);
            addAction("Added Data File: "+ newDataFile.getShortName(),save);
        }
        else {
            HelpDialog.warning("File has already been introduced","Warning");
        }
    }
    public static DataFile getDataFileByName(String name){
        for(DataFile dataFile : listOfDataFiles){
            if (dataFile.getShortName().equals(name)) {
                return(dataFile);
            }
        }
        return(null);
    }
    public static DataFile getFirstDataFile(){
        if(listOfDataFiles.size()>0){
            return(listOfDataFiles.get(0));
        }
        return(null);
    }
    public static List<DataFile> getListOfDataFiles() {
        return listOfDataFiles;
    }
    public static String getDataFileId(){
        for(String letter : Strings.getLetters()){
            for(DataFile dataFile : listOfDataFiles){
                if( ! letter.equals(dataFile.getId())){
                    return(letter);
                }
            }
        }
        return(Strings.getLetters()[0]);
    }

     // ------------------------------------------------------------------------------
    public static void saveExcel() { ExcelManager.saveExcel(); }

    public static void getExcel() { ExcelManager.getExcel(); }
    // ------------------------------------------------------------------------------

}
