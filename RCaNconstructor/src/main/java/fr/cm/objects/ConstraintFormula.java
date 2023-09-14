package fr.cm.objects;

import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ConstraintFormula {

    List<String> tokens = new ArrayList<>();
    // --------------------------------------------
    static List<String> nombres = new ArrayList<>(Arrays.asList(".", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",":"));
    // --------------------------------------------
    static List<String> operatorsArithmetiques = new ArrayList<>(Arrays.asList("+", "-", "*", "/"));
    // --------------------------------------------
    static List<String> operatorsUnaires =  new ArrayList<>(Arrays.asList("sum", "mean","survey"));
    // --------------------------------------------
    static List<String> operatorsComparaison = new ArrayList<>(Arrays.asList("<=", ">=","="));
    static List<String> shortcuts = new ArrayList<>(Arrays.asList("Outflows","Inflows","TrophicInflows","TrophicOuflows","TrophicAllFlows", "After","Before","Delta","Ratio"));
    // --------------------------------------------
    static List<String> symbolsAndOperators = new ArrayList<>(Arrays.asList("(", ")","[", "]", "+", "-", "*", "/", "<=", ">=","=","sum", "mean","survey"));
    // --------------------------------------------
    static List<String> symbolsAndOperatorsD = new ArrayList<>(Arrays.asList("(", ")", "[", "]", "+", "-", "*", "/", "&", "#","=","!", "?"));
    // --------------------------------------------
    List<String> operatorsBinaires = new ArrayList<>();
    boolean previousTokenIsNumeric;
    int pos;
    // --------------------------------------------
    public ConstraintFormula() {
        operatorsBinaires.addAll(operatorsArithmetiques);
        operatorsBinaires.addAll(operatorsComparaison);
        previousTokenIsNumeric = false;
        pos = -1;
    }

    // --------------------------------------------
    public ConstraintFormula(Constraint constraint) {
        String formulaString = constraint.getFormula();
        operatorsBinaires.addAll(operatorsArithmetiques);
        operatorsBinaires.addAll(operatorsComparaison);
        fromStringToTokens(formulaString);
        previousTokenIsNumeric = false;
        pos = tokens.size()-1;
    }

    // --------------------------------------------
    String codeOp(String st){
        return(st.replace("<=","&")
                .replace(">=","#")
                .replace("sum","?")
                .replace("mean","!")
        );
    }

    // --------------------------------------------
    String decodeOp(String st){
        return(st.replace("&","<=")
                .replace("#",">=")
                .replace("?","sum")
                .replace("!","mean")
        );
    }

    // --------------------------------------------
    public String fromTokensToString() {
        StringBuilder sb = new StringBuilder();
        for (String token : tokens) {
            sb.append(token).append(" ");
        }
        return (sb.toString());
    }

    // --------------------------------------------
    public void append(String token) {
        if (nombres.contains(token) && previousTokenIsNumeric) {
            String newToken = tokens.get(pos) + token;
            tokens.set(pos, newToken);
        }
        if (nombres.contains(token) && !previousTokenIsNumeric) {
            pos ++;
            tokens.add(pos, token);
            previousTokenIsNumeric = true;
         }
        if (!nombres.contains(token)) {
            pos ++;
            tokens.add(pos, token);
            previousTokenIsNumeric = false;
        }
    }

    // --------------------------------------------
    public void removeAt(int p) {
        tokens.remove(p);
        pos = Math.max(0, p - 1);
    }

    // --------------------------------------------
    public void clear() {
        previousTokenIsNumeric = false;
        tokens.clear();
        pos = -1;
    }

    // --------------------------------------------
    public void fromStringToTokens(String formulaString){
        boolean nameEnCours = false;
        boolean valueEnCours = false;
        tokens = new ArrayList<>();
        String codeFormulaString = codeOp(formulaString);
        CharacterIterator it = new StringCharacterIterator(codeFormulaString);

        String name = "";
        String value = "";
        while (it.current() != CharacterIterator.DONE) {
            Character ct = it.current();
            String t = String.valueOf(ct);
            if(symbolsAndOperatorsD.contains(t)) {
                if (nameEnCours) {
                    tokens.add(name);
                    name = "";
                    nameEnCours = false;
                }
                if (valueEnCours) {
                    tokens.add(value);
                    value = "";
                    valueEnCours = false;
                }
                tokens.add(decodeOp(t));
            }
            if(nameEnCours){
                if((ct >= 'A' && ct <= 'Z') || (ct >= 'a' && ct <= 'z') ||(ct >= '0' && ct <= '9') || (ct == '_') ) {
                    name = name + t;
                }
            } else {
                if ((ct >= 'A' && ct <= 'Z') || (ct >= 'a' && ct <= 'z')  ) {
                    nameEnCours = true;
                    valueEnCours = false;
                    name = t;
                }
            }
            if(valueEnCours) {
                if (nombres.contains(t)) {
                    value = value + t;
                }
            } else {
                if (nombres.contains(t) && ! nameEnCours) {
                    valueEnCours = true;
                    nameEnCours = false;
                    value = t;
                }
            }
            it.next();
        }
        if (nameEnCours) {
            tokens.add(name);
        }
        if (valueEnCours) {
            tokens.add(value);
        }
    }

    // ----------------------------------------------------------------------------------
    public String check() {
        // il ne doit y avoir qu un seul operateur de comparaison
        int nq = 0;
        for (String token : tokens) {
            if (operatorsComparaison.contains(token)) {
                nq++;
            }
        }
        if (!(nq == 1)) {
            return ("There is zero or more than one comparison operator");
        }
        // on remplace tout ce qui n'est pas opérateur ou symbole par ok
        // nombres -> ok
        // variables -> ok
        List<String> tokensOld = tokens;
        List<String> newTokens = new ArrayList<>();
        for (String s : tokensOld) {
            if (!symbolsAndOperators.contains(s)) {
                newTokens.add("ok");
            } else {
                newTokens.add(s);
            }
        }
        // on vérifie la validité de la formule par élimination successive des blocs corrects
        // un bloc correct est obtenu a partir des transformations
        // ok [ok] -> ok
        // operateur unaire ( ok ) -> ok
        // ( ok ) -> ok
        // ok operateurBinaire ok  -> ok
        tokensOld = newTokens;
        boolean changes = true;
        while (changes) {
            int nd = tokensOld.size();
            // ( ok ) -> ok
            newTokens = new ArrayList<>();
            for (int t = 0; t < tokensOld.size(); t++) {
                if (t > tokensOld.size() - 3) {
                    newTokens.add(tokensOld.get(t));
                } else {
                    if (tokensOld.get(t).equals("(")
                            && tokensOld.get(t + 1).equals("ok")
                            && tokensOld.get(t + 2).equals(")")
                    ) {
                        newTokens.add("ok");
                        t += 2;
                    } else {
                        newTokens.add(tokensOld.get(t));
                    }
                }
            }
            tokensOld = newTokens;
            // ok [ ok ] -> ok
            newTokens = new ArrayList<>();
            for (int t = 0; t < tokensOld.size(); t++) {
                if (t > tokensOld.size() - 3) {
                    newTokens.add(tokensOld.get(t));
                } else {
                    if (tokensOld.get(t).equals("[")
                            && tokensOld.get(t + 1).equals("ok")
                            && tokensOld.get(t + 2).equals("]")
                    ) {
                        // newTokens.add("ok");
                        t += 2;
                    } else {
                        newTokens.add(tokensOld.get(t));
                    }
                }
            }
            tokensOld = newTokens;
            // operateur unaire  ok  -> ok
            newTokens = new ArrayList<>();
            for (int t = 0; t < tokensOld.size(); t++) {
                if (t > tokensOld.size() - 2) {
                    newTokens.add(tokensOld.get(t));
                } else {
                    if (
                            operatorsUnaires.contains(tokensOld.get(t))
                                    && tokensOld.get(t + 1).equals("ok")
                    ) {
                        newTokens.add("ok");
                        t += 1;
                    } else {
                        newTokens.add(tokensOld.get(t));
                    }
                }
            }
            tokensOld = newTokens;
            // ok operateurBinaire ok  -> ok
            newTokens = new ArrayList<>();
            for (int t = 0; t < tokensOld.size(); t++) {
                if (t > tokensOld.size() - 3) {
                    newTokens.add(tokensOld.get(t));
                } else {
                    if (tokensOld.get(t).equals("ok") && operatorsBinaires.contains(tokensOld.get(t + 1)) && tokensOld.get(t + 2).equals("ok")) {
                        newTokens.add("ok");
                        t += 2;
                    } else {
                        newTokens.add(tokensOld.get(t));
                    }
                }
            }
            tokensOld = newTokens;
            // - ok  -> ok (- en première position)
            newTokens = new ArrayList<>();
            for (int t = 0; t < tokensOld.size(); t++) {
                if (t ==0 && tokensOld.get(0).equals("-")&& tokensOld.get(1).equals("ok")) {
                    newTokens.add("ok");
                } else {
                    newTokens.add(tokensOld.get(t));
                }
            }
            tokensOld = newTokens;
            int nf = tokensOld.size();
            changes = !(nf == nd);
        }
        if(tokensOld.size() !=  1){
            return ("Unbalanced formula");
        }
        return ("Ok");
    }
    // ----------------------------------------------------------------------------------
    public int getPos() { return pos; }
    public void setPos(int pos) { this.pos = Math.min(pos, tokens.size() - 1); }
    public int getNbTokens() { return (tokens.size()); }
    public String getToken(int t) { return (tokens.get(t)); }
    // ----------------------------------------------------------------------------------

}
