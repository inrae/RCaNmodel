package fr.cm.parameters;

import javafx.scene.control.Label;
import javafx.scene.control.MenuItem;
import org.apache.commons.math3.analysis.function.Add;

import java.util.Arrays;
import java.util.List;

public class Strings {
// ---------------------------------

    private static final String[] letters = new String[]{
            "A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U"};
    private static final String[] units = new String[]{
            "Million tons" ,
            "Thousands tons",
            "Tons",
            "Tons/Square meter",
            "No unit"
    };

    private static final String[] parametersNames = new String[]{
            "AssimilationE",
            "Digestibility",
            "OtherLosses",
            "Inertia",
            "Satiation",
            "RefugeBiomass"
    };

    private static final double[] parametersMaxValues = new double[]{
            -1.0,
            1.0,
            1.0,
            -1.0,
            1000.0,
            -1.0,
            // -1 signifie qu'il n'y a pas de valeur maximum
    };

    private static final String[] typesOfComponents = new String[]{
            "In",
            "Out"
    };
    private static final String[] typesOfLinks = new String[]{
            "Trophic",
            "Non trophic"
    };


    public static String[] getLetters(){
        return letters;
    }
    public static String[] getTypesOfComponents() { return (typesOfComponents); }

    public static String[] getTypesOfLinks() { return (typesOfLinks); }

    public static int getNumberOfParameters() { return parametersNames.length; }

    public static String getParametersNames(int p) { return parametersNames[p]; }

    public static String[] getParametersNames() { return parametersNames; }

    public static List<String> getUnits() { return Arrays.asList(units); }

    public static double[] getParametersMaxValues() {
        return (parametersMaxValues);
    }

    public static String anObservationWithThisNameAlreadyExists = "An observation with this name already exists";

    public static String addLinkHint = "If you add a link from this component, once this window is closed, click on destination compartement";

    public static String editComponent = "Edit component";
    public static String deleteComponent = "Delete component";
    public static String newTrophicLink = "New trophic link";
    public static String newNonTrophicLink = "New non trophic link";
    public static String cancel = "Cancel";
    public static String component = "Component";
    public static String numberFormatIssue = "Number format issue";
    public static String valueOutOfRange = "Value out of range";
    public static String componentAlreadyExists = "The component XXX already exists";
    public static String linkAlreadyExists = "The link XXX already exists";

    public static String addingAComponent = "Adding a component";
    public static String nameAndTypeOfNewComponent = "Enter the name and the type of a new component";
    public static String nameOfNewComponent = "Name of component";
    public static String typeOfNewComponent = "Type of component";

    public static String validityOfFormula = "Validity of formula : ";
    public static String addConstraint = "Add this constraint";
    public static String editConstraint = "Set edited constraint";
    public static String cancelConstraint = "Cancel";

    public static String clearFormula = "Clear Formula";
    public static String helpFormula = " - Click on one the above and left symbols or variables names" +
            "\n to add a first token or to add a token after the selected token (in red)," +
            "\n - Double click on a token to delete it" +
            "\n - Click on a token to select it." +
            "\n - Select years range of the constraint";

    public static String metaInformation = "Add information about this file; the authors; the reference; ect.";

    public static String fileWithObservationItem = "About input datafiles";

    // HELP MEMU
    public static String whatCaNHelp = "What is chance and Necessity modelling";
    public static String whatMetaHelp = "What is meta information";
    public static String whatConstraintHelp = "What are constraints";
    public static String whatObservationHelp = "What are observations";
    public static String whatFileWithObservationHelp = "What are files with observations";
    public static String viewNetworkHelp = "How to use the network map";
    public static String viewFileHelp = "How to use the file dialog";
    public static String viewCompartementHelp = "How to use the component view";
    public static String viewLinkHelp = "How to use the link view";
    public static String viewObservationHelp = "How to use the observation view";
    public static String viewConstraintHelp = "How to use the constraint view";
    public static String viewValidityFormulaHelp = "When is the formula of a constraint valid";
    public static String runRCaNHelp = "How to compute and analyze with RCaN+";
    public static String templatesHelp = "Files format";
}
