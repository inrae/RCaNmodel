package fr.cm.canObjects;

import fr.cm.network.NetworkView;

import java.util.ArrayList;
import java.util.List;

/**
 * @author christianmullon
 */
public class Project {

    private static List<Component> listOfComponents;
    private static List<Flux> listOfFluxes;
    private static List<Constraint> listOfConstraints;
    private static List<Observation> listOfObservations;
    private static List<ExternalDataFile> listOfExternalDataFiles;
    private static NetworkView networkView;

    //  ------------------------------------------------------------------------------
    // SYSTEM 
    public static void init() {
        listOfComponents = new ArrayList<>();
        listOfFluxes = new ArrayList<>();
        listOfConstraints = new ArrayList<>();
        listOfObservations = new ArrayList<>();
        listOfExternalDataFiles = new ArrayList<>();
        MetaInformation.init();
        networkView = new NetworkView();
        networkView.init();
        ListsManager.init(
                listOfComponents,
                listOfFluxes,
                listOfConstraints,
                listOfObservations,
                listOfExternalDataFiles
        );
    }

    public static NetworkView getNetworkView() {
        networkView.update();
        return networkView;
    }

}
