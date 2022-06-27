package fr.cm.canObjects;

import fr.cm.GUInetwork.NetworkView;
import fr.cm.RCaNMain.Context;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.excel.ExcelManager;

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
    private static MetaInformation metaInformation;

    private static NetworkView networkView;

    // --------------------------------------------

    public static void init() {
        networkView = new NetworkView();
        metaInformation = new MetaInformation();
        networkView.init();
        listOfComponents = new ArrayList<>();
        listOfFluxes = new ArrayList<>();
        listOfConstraints = new ArrayList<>();
        listOfObservations = new ArrayList<>();
        listOfDataFiles = new ArrayList<>();
        listOfActions = new ArrayList<>();
    }

    // ------------------------------------------------------------------------------
    // ACTIONS
    public static void addAction(Action action){
        listOfActions.add(action);
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

    // ------------------------------------------------------------------------------
    // META INFORMATION

    public static MetaInformation getMetaInformation() {
        return metaInformation;
    }

    public static void setMetaInformation(MetaInformation metaInformation) {
        ProjectListsManager.metaInformation = metaInformation;
    }

    // ------------------------------------------------------------------------------
    // NETWORK VIEW
    public static NetworkView getNetworkView() {
        networkView.update();
        return networkView;
    }
    // ------------------------------------------------------------------------------
    // COMPONENTS
    public static void addComponent(Component component) {
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
        Context.setChanged(true,"Added component: "+component.getName());
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
        Context.setChanged(true,"Removed component: "+component.getName());
    }
    public static boolean containsComponent(String groupName) {
        boolean contain = false;
        for (Component component : listOfComponents){
            if (groupName.equals(component.getName())) {
                contain = true;
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
            Context.setChanged(true);
        }
    }
    public static void downComponent(Component component) {
        int numComponent = listOfComponents.indexOf(component);
        if (numComponent < listOfComponents.size() - 1) {
            permuteListOfComponents(numComponent, numComponent + 1);
            Context.setChanged(true);
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
    public static void addLink(Flux flux) {
        if (containsLink(flux.getName())) {
            HelpDialog.warning("Link already exists","Warning");
        } else {
            flux.define(networkView.getMouseDoubleClickOnLineEventHandler());
            listOfFluxes.add(flux);
            Context.setChanged(true,"Added Flux: "+ flux.getName());
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
        Context.setChanged(true,"Removed Flux: "+ flux.getName());
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
            Context.setChanged(true);
        }
    }
    public static void downLink(Flux flux) {
        int numLink = listOfFluxes.indexOf(flux);
        if (numLink < listOfFluxes.size() - 1) {
            permuteListOfLinks(numLink, numLink + 1);
            Context.setChanged(true);
        }
    }
    public static List<Flux> getListOfFluxes() {
        return listOfFluxes;
    }

    // --------------------------------------------------------------------------
    //  OBSERVATIONS
    public static void addObservation(Observation observation) {
        if(containsObservation(observation.getObsName())){
            HelpDialog.warning("Observation already exists","Warning");
        } else if(containsComponent(observation.getObsName())){
            HelpDialog.warning("A component with same name already exists","Warning");
        }
            else {
            listOfObservations.add(observation);
             Context.setChanged(true,"Added Observation: "+ observation.getObsName());
        }
    }
     public static void removeObservation(String observationName){
        for (Observation observation : listOfObservations)
            if (observationName.equals(observation.getObsName())) {
                listOfObservations.remove(observation);
                Context.setChanged(true,"Removed Observation: "+ observation.getObsName());
                break;
            }
    }
    public static void removeObservation(Observation observation){
        listOfObservations.remove(observation);
        Context.setChanged(true,"Removed Observation: "+ observation.getObsName());
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
    public static void addConstraint(Constraint constraint) {
        listOfConstraints.add(constraint);
        Context.setChanged(true,"Added Constraint: "+ constraint.getName());
    }
    public static void removeConstraints(Constraint constraint) {
        listOfConstraints.remove(constraint);
        Context.setChanged(true,"Removed Constraint: "+ constraint.getName());
    }
    public static void updateConstraint(Constraint oldC, Constraint newC) {
        int pos = listOfConstraints.indexOf(oldC);
        listOfConstraints.set(pos,newC);
        Context.setChanged(true,"Updated Constraint: "+ oldC.getName());
    }
    public static void upConstraint(Constraint constraint) {
        int numConstraint = listOfConstraints.indexOf(constraint);
        if (numConstraint > 0) {
            permuteListOfConstraints(numConstraint, numConstraint - 1);
            Context.setChanged(true);
        }
    }
    public static void downConstraint(Constraint constraint) {
        int numConstraint = listOfConstraints.indexOf(constraint);
        if (numConstraint < listOfConstraints.size() - 1) {
            permuteListOfConstraints(numConstraint, numConstraint + 1);
            Context.setChanged(true);
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
    public static void addDataFile(DataFile newDataFile) {
        boolean in = false;
        for(DataFile dataFile : listOfDataFiles){
            if (dataFile.getShortName().equals(newDataFile.getShortName())) {
                in = true;
                break;
            }
        }
        if(! in) {
            listOfDataFiles.add(newDataFile);
            Context.setChanged(true,"Added Data File: "+ newDataFile.getShortName());
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
    public static List<DataFile> getListOfDataFiles() { return listOfDataFiles; }

     // ------------------------------------------------------------------------------
    public static void saveExcel() { ExcelManager.saveExcel(); }

    public static void getExcel() { ExcelManager.getExcel(); }
    // ------------------------------------------------------------------------------

}
