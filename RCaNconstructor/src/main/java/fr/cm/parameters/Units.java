package fr.cm.parameters;

public enum Units {

    MT("MT", "Millions tons") , TT("TT","Thousands tons"), T("T","Tons"),TSQ("TSQ","Tons/Square meter"),NU("NU","No unit");

    private String code;
    private String text;

    private Units(String code, String text) {
        this.code = code;
        this.text = text;
    }

    public static Units getByCode(String unitByCode) {
        for (Units g : Units.values()) {
            if (g.code.equals(unitByCode)) {
                return g;
            }
        }
        return null;
    }

    public String getCode() { return code; }

    public String getText() { return text; }

    @Override
    public String toString() { return this.text; }

}