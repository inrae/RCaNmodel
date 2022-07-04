package fr.cm.canObjects;

import fr.cm.RCaNMain.Context;

public class Action {

    String date;
    String comment;

    public Action(String comment){
        this.date = Context.getDate();
        this.comment = comment;
    }

    public Action(String date, String comment){
        this.date = date;
        this.comment = comment;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getComment() {
        return comment;
    }

    public void setComment(String comment) {
        this.comment = comment;
    }
}
