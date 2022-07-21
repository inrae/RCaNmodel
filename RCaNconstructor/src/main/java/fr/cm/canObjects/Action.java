package fr.cm.canObjects;

import fr.cm.RCaNMain.Context;

public class Action {

    String date;
    String comment;
    String commentAuthor;

    public Action(String comment){
        this.date = Context.getDate();
        this.comment = comment;
        this.commentAuthor="";
    }
    
    public Action(String comment, String commentAuthor){
        this.date = Context.getDate();
        this.comment = comment;
        this.commentAuthor=commentAuthor;
    }
    public Action(String date, String comment, String commentAuthor){
        this.date = date;
        this.comment = comment;
        this.commentAuthor=commentAuthor;
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

    public String getCommentAuthor() { return commentAuthor; }

    public void setCommentAuthor(String commentAuthor) { this.commentAuthor = commentAuthor;
    }
}
