package fr.cm.xmlFiles;


public class HelpXML {

    private String id;
    private String textMenu;
    private String subMenu;
    private String text;
    private String image;

    // ------------------------------------------------------------------------



    public HelpXML(String id,
                   String textMenu,
                   String subMenu,
                   String text,
                   String image) {
        this.id = nolf(id);
        this.textMenu = nolf(textMenu);
        this.subMenu = nolf(subMenu);
        this.text = text;
        this.image = nolf(image);
    }
    // ------------------------------------------------------------------------
    public String nolf(String st){ return(st.replace("\n","").trim()); }
    // ------------------------------------------------------------------------
    public void print(){
        System.out.println("MENU");
        System.out.println(this.id);
        System.out.println(this.textMenu);
        System.out.println(this.subMenu);
        System.out.println(this.text);
    }

    // ------------------------------------------------------------------------
    public String getTextMenu() { return textMenu; }

    public String getSubMenu() {
        return subMenu;
    }

    public String getId() { return id; }

    public String getText() { return text; }

    public String getImage() { return image; }
}
